-----------------------------------------------------------------------------
-- |
-- Module      : Network.Wai.Middleware.ThrottleV2
-- Description : WAI Request Throttling Middleware
-- Copyright   : (c) 2018 Daniel Fithian
-- License     : BSD3
-- Maintainer  : Daniel Fithian <daniel.m.fithian@gmail.com>
-- Stability   : experimental
-- Portability : POSIX
--
-- Uses a <https://en.wikipedia.org/wiki/Token_bucket Token Bucket>
-- algorithm (from the token-bucket package) to throttle WAI Requests.
--
--
-- == Example
--
-- @
-- FIXME
-- main = do
--   let expirationSpec = TimeSpec 5 0 -- five seconds
--   throttle <- newThrottle (defaultThrottleSettings expirationSpec) (const $ Right 1)
--   let appl = runThrottle throttle $ \ _ f -> f $
--         responseLBS ok200 [] "ok"
--   Warp.run 3000 app
-- @
module Network.Wai.Middleware.ThrottleV2 where

import Control.Concurrent.TokenBucket (TokenBucket, newTokenBucket, tokenBucketTryAlloc1)
import Control.Exception.Safe (onException)
import Control.Monad.STM (STM, atomically)
import Data.Cache (Cache, delete, insert, insertSTM, lookupSTM, newCache)
import Data.Hashable (Hashable)
import GHC.Word (Word64)
import Network.HTTP.Types (tooManyRequests429)
import Network.Wai (Application, Request, Response, responseLBS)
import System.Clock (Clock (Monotonic), TimeSpec, getTime)

data CacheState a
  = CacheStatePresent a
  | CacheStateInitializing

data CacheResult a
  = CacheResultExists a
  | CacheResultEmpty

-- |A throttle for a hashable key type. Initialize using 'newThrottle' with 'defaultThrottleSettings'.
data Throttle a = Throttle
  { throttleSettings :: ThrottleSettings
  -- ^ The throttle settings
  , throttleCache    :: Cache a (CacheState TokenBucket)
  -- ^ The cache, initialized in 'newThrottle'
  , throttleGetKey   :: Request -> Either Response a
  -- ^ The function to extract a throttle key from a 'Network.Wai.Request'
  }

-- |Throttle settings for controlling token bucket algorithm and cache expiration.
data ThrottleSettings = ThrottleSettings
  { throttleSettingsRate            :: Double
  -- ^ Number of requests per throttle period allowed, defaults to 1
  , throttleSettingsPeriod          :: Double
  -- ^ Microseconds, defaults to 1 second
  , throttleSettingsBurst           :: Word64
  -- ^ Number of concurrent requests allowed - should be greater than rate / period, defaults to 1
  , throttleSettingsCacheExpiration :: TimeSpec
  -- ^ The amount of time before a stale token bucket is purged from the cache
  , throttleSettingsIsThrottled     :: Request -> Bool
  -- ^ Whether or not a request is throttled, defaults to true
  , throttleSettingsOnThrottled     :: Word64 -> Response
  -- ^ The response when a request is throttled - defaults to a vanilla 429
  }

-- |Default throttle settings.
defaultThrottleSettings :: TimeSpec -> ThrottleSettings
defaultThrottleSettings expirationInterval = ThrottleSettings
  { throttleSettingsRate = 1
  , throttleSettingsPeriod = 1000000
  , throttleSettingsBurst = 1
  , throttleSettingsCacheExpiration = expirationInterval
  , throttleSettingsIsThrottled = const True
  , throttleSettingsOnThrottled = const $ responseLBS tooManyRequests429 [("Content-Type", "application/json")] "Too many requests."
  }

-- |Initialize a throttle using settings and a way to extract the key from the request.
newThrottle :: ThrottleSettings -> (Request -> Either Response a) -> IO (Throttle a)
newThrottle throttleSettings@(ThrottleSettings {..}) throttleGetKey = do
  throttleCache <- newCache $ Just throttleSettingsCacheExpiration
  pure Throttle {..}

-- |Internal use only. Retrieve a token bucket from the cache.
retrieveCache :: (Eq a, Hashable a) => Throttle a -> TimeSpec -> a -> STM (CacheResult TokenBucket)
retrieveCache throttle time throttleKey = do
  let cache = throttleCache throttle
  lookupSTM True throttleKey cache time >>= \ case
    Just (CacheStatePresent oldBucket) -> pure $ CacheResultExists oldBucket
    Just CacheStateInitializing -> retrieveCache throttle time throttleKey
    Nothing -> do
      insertSTM throttleKey CacheStateInitializing cache Nothing
      pure CacheResultEmpty

-- |Internal use only. Create a token bucket if it wasn't in the cache.
processCacheResult :: (Eq a, Hashable a) => Throttle a -> a -> CacheResult TokenBucket -> IO TokenBucket
processCacheResult throttle throttleKey cacheResult = case cacheResult of
  CacheResultExists bucket -> pure bucket
  CacheResultEmpty -> do
    let cache = throttleCache throttle
        initializeBucket = do
          bucket <- newTokenBucket
          insert cache throttleKey (CacheStatePresent bucket)
          pure bucket
        cleanupBucket = delete cache throttleKey
    initializeBucket `onException` cleanupBucket

-- |Internal use only. Retrieve or initialize a token bucket depending on if it was found in the cache.
retrieveOrInitializeBucket :: (Eq a, Hashable a) => Throttle a -> a -> IO TokenBucket
retrieveOrInitializeBucket throttle throttleKey = do
  now <- getTime Monotonic
  cacheResult <- atomically $ retrieveCache throttle now throttleKey
  processCacheResult throttle throttleKey cacheResult

-- |Internal use only. Throttle a request by the throttle key.
throttleRequest :: (Eq a, Hashable a) => Throttle a -> a -> IO Word64
throttleRequest throttle throttleKey = do
  bucket <- retrieveOrInitializeBucket throttle throttleKey
  let settings = throttleSettings throttle
      rate = throttleSettingsRate settings
      period = throttleSettingsPeriod settings
      burst = throttleSettingsBurst settings
  tokenBucketTryAlloc1 bucket burst $ round (period / rate)

-- |Run the throttling middleware given a throttle that has been initialized.
runThrottle :: (Eq a, Hashable a) => Throttle a -> Application -> Application
runThrottle throttle app req respond = do
  let settings = throttleSettings throttle
      getKey = throttleGetKey throttle
      isThrottled = throttleSettingsIsThrottled settings
      onThrottled = throttleSettingsOnThrottled settings
  case isThrottled req of
    False -> app req respond
    True -> case getKey req of
      Left response     -> respond response
      Right throttleKey -> do
        throttleRequest throttle throttleKey >>= \ case
          0 -> app req respond
          n -> respond $ onThrottled n

instance Show (CacheState a) where
  show = \ case
    CacheStatePresent _ -> "Present"
    CacheStateInitializing -> "Initializing"

instance Show (CacheResult a) where
  show = \ case
    CacheResultExists _ -> "Exists"
    CacheResultEmpty -> "Empty"
