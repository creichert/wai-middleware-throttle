module Network.Wai.Middleware.Throttle.Internal where

import Prelude hiding (lookup)

import Control.Concurrent.TokenBucket (TokenBucket, newTokenBucket, tokenBucketTryAlloc1)
import Control.Exception.Safe (onException)
#if MIN_VERSION_cache(0,1,1)
import Control.Monad.STM (STM, atomically)
import Data.Cache (Cache, delete, insert, insertSTM, lookupSTM, newCache)
#else
import Data.Cache (Cache, delete, insert, insert', lookup, newCache)
#endif
import Data.Hashable (Hashable, hashWithSalt)
import GHC.Word (Word64)
import Network.HTTP.Types.Status (status429)
import Network.Socket (SockAddr (SockAddrInet, SockAddrInet6, SockAddrUnix))
#if MIN_VERSION_network(2,6,1)
import Network.Socket (SockAddr (SockAddrCan))
#endif
import Network.Wai (Application, Request, Response, remoteHost, responseLBS)
import System.Clock (Clock (Monotonic), TimeSpec, getTime)

newtype Address = Address SockAddr

instance Hashable Address where
  hashWithSalt s (Address (SockAddrInet _ a))      = hashWithSalt s a
  hashWithSalt s (Address (SockAddrInet6 _ _ a _)) = hashWithSalt s a
  hashWithSalt s (Address (SockAddrUnix a))        = hashWithSalt s a
#if MIN_VERSION_network(2,6,1)
  hashWithSalt s (Address (SockAddrCan a))         = hashWithSalt s a
#endif

instance Eq Address where
  Address (SockAddrInet _ a)      == Address (SockAddrInet _ b)      = a == b
  Address (SockAddrInet6 _ _ a _) == Address (SockAddrInet6 _ _ b _) = a == b
  Address (SockAddrUnix a)        == Address (SockAddrUnix b)        = a == b
#if MIN_VERSION_network(2,6,1)
  Address (SockAddrCan a)         == Address (SockAddrCan b)         = a == b
#endif
  _ == _ = False -- not same constructor so cant be equal

instance Ord Address where
  Address (SockAddrInet _ a)      <= Address (SockAddrInet _ b)      = a <= b
  Address (SockAddrInet6 _ _ a _) <= Address (SockAddrInet6 _ _ b _) = a <= b
  Address (SockAddrUnix a)        <= Address (SockAddrUnix b)        = a <= b
#if MIN_VERSION_network(2,6,1)
  Address (SockAddrCan a)         <= Address (SockAddrCan b)         = a <= b
#endif
  Address a <= Address b = a <= b -- not same constructor so use builtin ordering

extractAddress :: Request -> Either Response Address
extractAddress = Right . Address . remoteHost

data CacheState a
  = CacheStatePresent a
  | CacheStateInitializing

data CacheResult a
  = CacheResultExists a
  | CacheResultEmpty

-- |A throttle for a hashable key type. Initialize using 'initThrottler' with 'defaultThrottleSettings'.
data Throttle a = Throttle
  { throttleSettings :: ThrottleSettings
  -- ^ The throttle settings
  , throttleCache    :: Cache a (CacheState TokenBucket)
  -- ^ The cache, initialized in 'initThrottler'
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
  , throttleSettingsOnThrottled = const $
      responseLBS status429 [("Content-Type", "application/json")] "{\"message\":\"Too many requests.\"}"
  }

initThrottler :: ThrottleSettings -> IO (Throttle Address)
initThrottler = flip initCustomThrottler extractAddress

-- |Initialize a throttle using settings and a way to extract the key from the request.
initCustomThrottler :: ThrottleSettings -> (Request -> Either Response a) -> IO (Throttle a)
initCustomThrottler throttleSettings@(ThrottleSettings {..}) throttleGetKey = do
  throttleCache <- newCache $ Just throttleSettingsCacheExpiration
  pure Throttle {..}

-- |Internal use only. Retrieve a token bucket from the cache.
#if MIN_VERSION_cache(0,1,1)
retrieveCache :: (Eq a, Hashable a) => Throttle a -> TimeSpec -> a -> STM (CacheResult TokenBucket)
retrieveCache th time throttleKey = do
  let cache = throttleCache th
  lookupSTM True throttleKey cache time >>= \ case
    Just (CacheStatePresent oldBucket) -> pure $ CacheResultExists oldBucket
    Just CacheStateInitializing -> retrieveCache th time throttleKey
    Nothing -> do
      insertSTM throttleKey CacheStateInitializing cache Nothing
      pure CacheResultEmpty
#else
retrieveCache :: (Eq a, Hashable a) => Throttle a -> TimeSpec -> a -> IO (CacheResult TokenBucket)
retrieveCache th time throttleKey = do
  let cache = throttleCache th
  lookup cache throttleKey >>= \ case
    Just (CacheStatePresent oldBucket) -> pure $ CacheResultExists oldBucket
    Just CacheStateInitializing -> retrieveCache th time throttleKey
    Nothing -> do
      insert' cache Nothing throttleKey CacheStateInitializing
      pure CacheResultEmpty
#endif

-- |Internal use only. Create a token bucket if it wasn't in the cache.
processCacheResult :: (Eq a, Hashable a) => Throttle a -> a -> CacheResult TokenBucket -> IO TokenBucket
processCacheResult th throttleKey cacheResult = case cacheResult of
  CacheResultExists bucket -> pure bucket
  CacheResultEmpty -> do
    let cache = throttleCache th
        initializeBucket = do
          bucket <- newTokenBucket
          insert cache throttleKey (CacheStatePresent bucket)
          pure bucket
        cleanupBucket = delete cache throttleKey
    initializeBucket `onException` cleanupBucket

-- |Internal use only. Retrieve or initialize a token bucket depending on if it was found in the cache.
retrieveOrInitializeBucket :: (Eq a, Hashable a) => Throttle a -> a -> IO TokenBucket
retrieveOrInitializeBucket th throttleKey = do
  now <- getTime Monotonic
#if MIN_VERSION_cache(0,1,1)
  cacheResult <- atomically $ retrieveCache th now throttleKey
#else
  cacheResult <- retrieveCache th now throttleKey
#endif
  processCacheResult th throttleKey cacheResult

-- |Internal use only. Throttle a request by the throttle key.
throttleRequest :: (Eq a, Hashable a) => Throttle a -> a -> IO Word64
throttleRequest th throttleKey = do
  bucket <- retrieveOrInitializeBucket th throttleKey
  let settings = throttleSettings th
      rate = throttleSettingsRate settings
      period = throttleSettingsPeriod settings
      burst = throttleSettingsBurst settings
  tokenBucketTryAlloc1 bucket burst $ round (period / rate)

-- |Run the throttling middleware given a throttle that has been initialized.
throttle :: (Eq a, Hashable a) => Throttle a -> Application -> Application
throttle th app req respond = do
  let settings = throttleSettings th
      getKey = throttleGetKey th
      isThrottled = throttleSettingsIsThrottled settings
      onThrottled = throttleSettingsOnThrottled settings
  case isThrottled req of
    False -> app req respond
    True -> case getKey req of
      Left response     -> respond response
      Right throttleKey -> do
        throttleRequest th throttleKey >>= \ case
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
