module WaiMiddlewareThrottleV2Spec where

import Prelude hiding (lookup)

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Cache (lookup)
import Data.Maybe (isJust, isNothing)
import System.Clock (TimeSpec (TimeSpec))
import Test.Hspec (Spec, before, describe, it, shouldSatisfy)

-- the module being tested
import Network.Wai.Middleware.ThrottleV2

spec :: Spec
spec = do
  let expirationSpec = TimeSpec 5 0
      runBefore :: IO (Throttle Int)
      runBefore = newThrottle (defaultThrottleSettings expirationSpec) (const $ Right 1)
  before runBefore $
    describe "Request Throttling" $ do
      describe "Bucket Operations" $ do
        it "initializes bucket when missing" $ \ throttle -> do
          let throttleKey = 1
              cache = throttleCache throttle
          void $ retrieveOrInitializeBucket throttle throttleKey
          lookup cache throttleKey >>= \ b -> b `shouldSatisfy` isJust
        it "retrieves bucket on subsequent calls" $ \ throttle -> do
          let throttleKey = 1
              cache = throttleCache throttle
          void $ retrieveOrInitializeBucket throttle throttleKey
          void $ retrieveOrInitializeBucket throttle throttleKey
          lookup cache throttleKey >>= \ b -> b `shouldSatisfy` isJust
        it "expires buckets" $ \ throttle -> do
          let throttleKey = 1
              cache = throttleCache throttle
          void $ retrieveOrInitializeBucket throttle throttleKey
          lookup cache throttleKey >>= \ b -> b `shouldSatisfy` isJust
          threadDelay 5000000
          lookup cache throttleKey >>= \ b -> b `shouldSatisfy` isNothing
