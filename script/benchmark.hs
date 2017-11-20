module Main where

import ClassyPrelude
import Control.Concurrent (ThreadId, forkIO)
import Control.Lens ((&), (.~), (?~))
import Control.Monad.Except (throwError)
import Criterion (Benchmark, bench, bgroup, whnfIO)
import Criterion.Main (defaultMain)
import Data.Text (strip)
import Network.HTTP.Types (hAuthorization, status200)
import Network.Wai (requestBody, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Throttle
  ( CustomWaiThrottle, RequestHashable, ThrottleSettings
  , defaultThrottleSettings, initCustomThrottler, initThrottler, requestToKey
  , throttle, throttlePeriod )
import Network.Wreq ( defaults, getWith, header, postWith
#if !MIN_VERSION_wreq(0,5,0)
                    , checkStatus
#else
                    , checkResponse
#endif
                    )

newtype Key = Key { unKey :: Text }
  deriving (Eq, Ord, Hashable)

instance RequestHashable Key where
  requestToKey =
    let authorization = find ((== hAuthorization) . fst) . requestHeaders
          >=> stripPrefix "basic" . toCaseFold . decodeUtf8 . snd
          >=> Just . Key . strip
    in maybe (throwError "No authorization header") pure . authorization

serverWithThrottle :: RequestHashable a => ThrottleSettings -> CustomWaiThrottle a -> Int -> IO ThreadId
serverWithThrottle settings th port =
  let app = throttle settings th $ \ x f ->
        f . responseLBS status200 [] . fromStrict =<< requestBody x
  in forkIO $ run port app

benchmark :: String -> Int -> Benchmark
benchmark name port =
  let endpoint = "http://localhost:" <> show port <> "/"
      options = defaults &
        header hAuthorization .~ ["BASIC foo"] &
#if !MIN_VERSION_wreq(0,5,0)
        checkStatus ?~ (\ _ _ _ -> Nothing)
#else
        checkResponse ?~ (\ _ _ -> pure ())
#endif
      doPost = postWith options endpoint ("foo" :: ByteString)
      doGet = getWith options endpoint
  in bgroup name [ bench "get" $ whnfIO doGet
                 , bench "post" $ whnfIO doPost ]

main :: IO ()
main = do
  let longPeriod = defaultThrottleSettings { throttlePeriod = 30000000 }
      shortPeriod = defaultThrottleSettings { throttlePeriod = 1 }
  ipTh <- initThrottler
  keyTh <- initCustomThrottler :: IO (CustomWaiThrottle Key)
  void $ serverWithThrottle defaultThrottleSettings ipTh 3001
  void $ serverWithThrottle defaultThrottleSettings keyTh 3002
  void $ serverWithThrottle longPeriod ipTh 3003
  void $ serverWithThrottle longPeriod keyTh 3004
  void $ serverWithThrottle shortPeriod ipTh 3005
  void $ serverWithThrottle shortPeriod keyTh 3006

  putStrLn "benchmark"
  defaultMain [ benchmark "default by ip" 3001
              , benchmark "default by key" 3002
              , benchmark "long by ip" 3003
              , benchmark "long by key" 3004
              , benchmark "short by ip" 3005
              , benchmark "long by key" 3006 ]
