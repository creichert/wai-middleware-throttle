module WaiMiddlewareThrottleSpec where

import Prelude hiding (lookup)

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM, void)
import Data.Cache (lookup)
import Data.Maybe (isJust, isNothing)
import Network.HTTP.Types.Status (status200, status429)
import Network.Wai (defaultRequest, responseLBS)
import Network.Wai.Test (request, runSession, simpleStatus)
import System.Clock (TimeSpec (TimeSpec))
import Test.Hspec (Spec, before, describe, it, shouldSatisfy)

-- the modules being tested
import Network.Wai.Middleware.Throttle
import Network.Wai.Middleware.Throttle.Internal

spec :: Spec
spec = do
  let expirationSpec = TimeSpec 5 0
      runBefore :: IO (Throttle Int)
      runBefore = initThrottler (defaultThrottleSettings expirationSpec) (const $ Right 1)
  before runBefore $
    describe "Network.Wai.Middleware.Throttle" $ do
      describe "Bucket Operations" $ do

        it "initializes bucket when missing" $ \ th -> do
          let throttleKey = 1
              cache = throttleCache th
          void $ retrieveOrInitializeBucket th throttleKey
          lookup cache throttleKey >>= \ b -> b `shouldSatisfy` isJust

        it "retrieves bucket on subsequent calls" $ \ th -> do
          let throttleKey = 1
              cache = throttleCache th
          void $ retrieveOrInitializeBucket th throttleKey
          void $ retrieveOrInitializeBucket th throttleKey
          lookup cache throttleKey >>= \ b -> b `shouldSatisfy` isJust

        it "expires buckets" $ \ th -> do
          let throttleKey = 1
              cache = throttleCache th
          void $ retrieveOrInitializeBucket th throttleKey
          lookup cache throttleKey >>= \ b -> b `shouldSatisfy` isJust
          threadDelay 5000000
          lookup cache throttleKey >>= \ b -> b `shouldSatisfy` isNothing

      describe "Throttling Behavior" $ do

        it "throttles requests" $ \ th -> do
          let appl = throttle th $ \ _ f -> f $
                responseLBS status200 [] "ok"
          statuses <- flip runSession appl $ do
            responses <- replicateM 100 (request defaultRequest)
            pure $ simpleStatus <$> responses
          statuses `shouldSatisfy` elem status429
