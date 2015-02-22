------------------------------------------------------------------------
-- |
-- Module      : WaiMiddlewareThrottleSpec
-- Description : WAI Request Throttling Middleware
-- Copyright   : (c) 2015 Christopher Reichert
-- License     : BSD3
-- Maintainer  : Christopher Reichert <creichert07@gmail.com>
-- Stability   : unstable
-- Portability : POSIX
--
{-# LANGUAGE OverloadedStrings #-}

module WaiMiddlewareThrottleSpec (
    spec
  ) where

import           Control.Monad.IO.Class
import           Network.HTTP.Types
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Test
import           Test.Hspec
import           Test.HUnit                      hiding (Test)

import           Network.Wai.Middleware.Throttle


spec :: Spec
spec = describe "Network.Wai.Middleware.Throttle" $
    it "throttles requests" caseThrottle


-- | Simple Hmac Middleware App
--
-- This app has preloaded api keys to simulate
-- some database or service which can access the
-- private keys.
throttleApp :: WaiThrottle -> Application
throttleApp st = throttle defaultThrottleSettings st
                   $ \_ f -> f response
  where
    payload  = "{ \"api\", \"return data\" }"
    response = responseLBS status200 [] payload


defReq :: Request
defReq  = defaultRequest
            { requestMethod  = "GET"
            , requestHeaders = [ ("Content-Type", "application/json") ]
            }


-- | Test Hmac Authentication
caseThrottle :: Assertion
caseThrottle = do

  st <- liftIO initThrottler

  statuses <- flip runSession (throttleApp st) $ do
    responses <- mapM (const (request defReq)) [ 1 .. 100 :: Integer ]
    mapM (return . simpleStatus) responses

  let msg = "Verifying some of the requests were throttled"
  assertBool msg  $ elem status429 statuses
