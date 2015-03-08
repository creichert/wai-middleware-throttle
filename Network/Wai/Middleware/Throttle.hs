-----------------------------------------------------------------------------
-- |
-- Module      : Network.Wai.Middleware.Throttle
-- Description : WAI Request Throttling Middleware
-- Copyright   : (c) 2015 Christopher Reichert
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Middleware.Throttle (

      -- | Wai Request Throttling Middleware
      throttle

      -- | Wai Throttle middleware state.
      --
      -- Essentially, a TVar with a HashMap for indexing
      -- remote IP address
    , WaiThrottle
    , initThrottler

      -- | Throttle settings and configuration
    , ThrottleSettings(..)
    , defaultThrottleSettings
    ) where

import           Control.Concurrent.STM
import           Control.Concurrent.TokenBucket
import           Control.Monad                  (liftM)
import qualified Data.ByteString.Char8          as BS
import qualified Data.IntMap                    as IM
import           GHC.Word                       (Word64)
import qualified Network.HTTP.Types.Status      as Http
import           Network.Socket
import           Network.Wai


newtype WaiThrottle = WT (TVar ThrottleState)


-- | A 'HashMap' mapping the remote IP address to a 'TokenBucket'
data ThrottleState = ThrottleState !(IM.IntMap TokenBucket)


-- | Settings which control various behaviors in the middleware.
data ThrottleSettings = ThrottleSettings
    {
      -- | Determines whether the 'Request' is throttled
      isThrottled   :: !(Request -> IO Bool)

      -- | Function to run when the request is throttled.
      --
      -- The first argument is a 'Word64' containing the amount
      -- of microseconds until the next retry should be attempted
    , onThrottled   :: !(Word64 -> Response)

      -- Zone name
      --
      -- TODO use list of zones (rules)
      -- , throttleZone      :: !T.Text

      -- | Rate
    , throttleRate  :: !Integer  -- requests / second

      -- Maximum size of the address cache in MB (similar to nginx)
      --
      -- You can store approximately 160,000 addresses in 10MB with
      -- \$binary_remote_addr.
      -- , throttleCacheSize :: !Integer

      -- | Burst rate
    , throttleBurst :: !Integer
    }


initThrottler :: IO WaiThrottle
initThrottler = liftM WT $ newTVarIO $ ThrottleState IM.empty


-- | Default settings to throttle requests.
defaultThrottleSettings :: ThrottleSettings
defaultThrottleSettings
    = ThrottleSettings {
        isThrottled         = return . const True
        -- , throttleZone        = "" -- empty zone
      , throttleRate        = 1  -- req / sec
        -- , throttleCacheSize   = 10 -- 10M address cache
      , throttleBurst       = 1  -- 5 concurrent requests
      , onThrottled         = onThrottled'
      }
  where
    bshow = BS.pack . show
    -- remaining = bshow (if 5000 - c < 0
    --                      then 0
    --                      else 5000 - c)
    onThrottled' rt =
      responseLBS
        Http.status429
        [ ("Content-Type", "application/json")
          -- , ("X-RateLimit-Limit", "5000")
          -- , ("X-RateLimit-Remaining", remaining)
        , ("X-RateLimit-Reset",
             bshow (fromIntegral rt / 1000000.0 :: Double))
        ]
        -- match YesodAuth error message renderer
        "{\"message\":\"Too many requests.\"}"


-- | WAI Request Throttling Middleware.
--
-- Uses a 'Request's 'remoteHost' function to resolve the
-- remote IP address.
throttle :: ThrottleSettings
            -> WaiThrottle
            -> Application
            -> Application
throttle ThrottleSettings{..} (WT tmap) app req respond = do

    -- determine whether the request needs throttling
    reqIsThrottled <- isThrottled req

    -- seconds remaining (if the request failed), 0 otherwise.
    remaining <- if reqIsThrottled
                   then throttleReq
                   else return 0

    if remaining /= 0
        then respond $ onThrottled remaining
        else app req respond
  where
    throttleReq = do

      let SockAddrInet _ remoteAddr = remoteHost req
      throttleState  <- atomically $ readTVar tmap
      (tst, success) <- throttleReq' (fromIntegral remoteAddr) throttleState

      -- write the throttle state back
      atomically $ writeTVar tmap (ThrottleState tst)
      return success

    throttleReq' remoteAddr (ThrottleState m) = do

      let toInvRate r = round (1e6 / r)
          invRate     = toInvRate (fromInteger throttleRate :: Double)
          burst       = fromInteger throttleBurst

      bucket    <- maybe newTokenBucket return $ IM.lookup remoteAddr m
      remaining <- tokenBucketTryAlloc1 bucket burst invRate

      return (IM.insert remoteAddr bucket m, remaining)
