-----------------------------------------------------------------------------
-- |
-- Module      : Network.Wai.Middleware.Throttle
-- Description : WAI Request Throttling Middleware
-- Copyright   : (c) 2015-2017 Christopher Reichert
-- License     : BSD3
-- Maintainer  : Christopher Reichert <creichert07@gmail.com>
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
-- main = do
--   st <- initThrottler
--   let payload  = "{ \"api\": \"return data\" }"
--       app = throttle defaultThrottleSettings st
--               $ \_ f -> f (responseLBS status200 [] payload)
--   Warp.run 3000 app
-- @

module Network.Wai.Middleware.Throttle (

      -- | Wai Request Throttling Middleware
      throttle

      -- | Wai Throttle middleware state.
      --
      -- Essentially, a TVar with a HashMap for indexing
      -- remote IP address
    , WaiThrottle, CustomWaiThrottle
    , initThrottler, initCustomThrottler

      -- | Throttle settings and configuration
    , ThrottleSettings(..)
    , defaultThrottleSettings

    , RequestHashable(..)
    ) where

import           Control.Applicative            ((<$>), pure)
import           Control.Concurrent.STM
import           Control.Concurrent.TokenBucket
import           Control.Monad                  (join, liftM)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Except           (ExceptT, runExceptT)
import           Data.ByteString.Builder        (stringUtf8, toLazyByteString)
import           Data.Function                  (on)
import           Data.Hashable                  (Hashable, hash, hashWithSalt)
import qualified Data.IntMap                    as IM
import           Data.List                      (unionBy)
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text, unpack)
import           GHC.Word                       (Word64)
import qualified Network.HTTP.Types.Status      as Http
import           Network.Socket
import           Network.Wai

#ifndef MIN_VERSION_network
#define MIN_VERSION_network(a,b,c) 1
#endif

newtype CustomWaiThrottle a = WT (TVar (ThrottleState a))
type WaiThrottle = CustomWaiThrottle Address


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

-- | A 'HashMap' mapping the remote IP address to a 'TokenBucket'
data ThrottleState a = ThrottleState !(IM.IntMap [(a,TokenBucket)])


-- | Settings which control various behaviors in the middleware.
data ThrottleSettings = ThrottleSettings
    {
      -- | Determines whether the 'Request' is throttled
      isThrottled    :: !(Request -> IO Bool)

      -- | Function to run when the request is throttled.
      --
      -- The first argument is a 'Word64' containing the amount
      -- of microseconds until the next retry should be attempted
    , onThrottled    :: !(Word64 -> Response)
    , onRequestError :: !(Text -> Response)

      -- | Rate
    , throttleRate   :: !Integer  -- requests / throttlePeriod
    , throttlePeriod :: !Integer -- microseconds

      -- | Burst rate
    , throttleBurst  :: !Integer
    }


initThrottler :: IO WaiThrottle
initThrottler = initCustomThrottler

initCustomThrottler :: IO (CustomWaiThrottle a)
initCustomThrottler = liftM WT $ newTVarIO $ ThrottleState IM.empty


-- | Default settings to throttle requests.
defaultThrottleSettings :: ThrottleSettings
defaultThrottleSettings
    = ThrottleSettings {
        isThrottled         = return . const True
      , throttleRate        = 1  -- req / throttlePeriod
      , throttlePeriod      = 10^6 -- microseconds
      , throttleBurst       = 1  -- concurrent requests
      , onThrottled         = onThrottled'
      , onRequestError      = onRequestError'
      }
  where
    onThrottled' _ =
      responseLBS
        Http.status429
        [ ("Content-Type", "application/json")
        ]
        "{\"message\":\"Too many requests.\"}"
    onRequestError' reason =
      responseLBS
        Http.status400
        [ ("Content-Type", "application/json")
        ]
        ("{\"message\":\"" <> toLazyByteString (stringUtf8 $ unpack reason) <> "\"}")

class (Eq a, Ord a, Hashable a) => RequestHashable a where
  requestToKey :: (Functor m, Monad m) => Request -> ExceptT Text m a

instance RequestHashable Address where
  requestToKey = pure . Address . remoteHost

-- | WAI Request Throttling Middleware.
--
-- Uses a 'Request's 'remoteHost' function to resolve the
-- remote IP address.
throttle :: RequestHashable a
         => ThrottleSettings
         -> CustomWaiThrottle a
         -> Application
         -> Application
throttle ThrottleSettings{..} (WT tmap) app req respond = do

    -- determine whether the request needs throttling
    reqIsThrottled <- isThrottled req

    -- seconds remaining (if the request failed), 0 otherwise.
    remaining <- if reqIsThrottled
                   then runExceptT throttleReq
                   else return $ Right 0

    case remaining of
      Left err -> respond $ onRequestError err
      Right 0 -> app req respond
      Right n -> respond $ onThrottled n
  where
    throttleReq = do

      k <- requestToKey req
      throttleState  <- liftIO . atomically $ readTVar tmap
      (tst, success) <- liftIO $ throttleReq' k throttleState

      -- write the throttle state back
      liftIO . atomically $ writeTVar tmap (ThrottleState tst)
      return success

    throttleReq' k (ThrottleState m) = do

      let toInvRate r = round (period / r)
          period      = (fromInteger throttlePeriod :: Double)
          invRate     = toInvRate (fromInteger throttleRate :: Double)
          burst       = fromInteger throttleBurst

      bucket    <- maybe newTokenBucket return $ join $ lookup k <$> IM.lookup (hash k) m
      remaining <- tokenBucketTryAlloc1 bucket burst invRate

      return (insertBucket k bucket m, remaining)

    insertBucket k bucket m =
      let col = unionBy ((==) `on` fst)
      in IM.insertWith col (hash k) [(k, bucket)] m
