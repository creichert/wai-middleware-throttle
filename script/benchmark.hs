{-
simple benchmarking tool:
stack install
benchmark -o bench.html (or benchmark --json bench.json)
-}
import Prelude

import Control.Concurrent (ThreadId, forkIO)
import Control.Lens ((&), (.~), (?~), over, _Left)
import Control.Monad ((>=>), void)
import Control.Monad.Except (throwError, runExcept)
import Criterion (Benchmark, bench, bgroup, whnfIO)
import Criterion.Main (defaultMain)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.Hashable (Hashable)
import Data.Foldable (find)
import Data.Text (Text, strip, stripPrefix, toCaseFold, unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (hAuthorization, status200, status400)
import Network.Wai (requestBody, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Middleware.Throttle as Throttle
import qualified Network.Wai.Middleware.ThrottleV2 as ThrottleV2
import Network.Wreq ( defaults, getWith, header, postWith
#if !MIN_VERSION_wreq(0,5,0)
                    , checkStatus
#else
                    , checkResponse
#endif
                    )
import System.Clock (TimeSpec (TimeSpec))

newtype Key = Key Text
  deriving (Eq, Ord, Hashable)

instance Throttle.RequestHashable Key where
  requestToKey =
    let authorization = find ((== hAuthorization) . fst) . requestHeaders
          >=> stripPrefix "basic" . toCaseFold . decodeUtf8 . snd
          >=> Just . Key . strip
    in maybe (throwError "No authorization header") pure . authorization

serverWithThrottle :: Throttle.RequestHashable a => Throttle.ThrottleSettings -> Throttle.CustomWaiThrottle a -> Int -> IO ThreadId
serverWithThrottle settings th port =
  let app = Throttle.throttle settings th $ \ x f ->
        f . responseLBS status200 [] . fromStrict =<< requestBody x
  in forkIO $ run port app

serverWithThrottleV2 :: (Eq a, Hashable a) => ThrottleV2.Throttle a -> Int -> IO ThreadId
serverWithThrottleV2 th port =
  let app = ThrottleV2.runThrottle th $ \ x f ->
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
  let expirationInterval = TimeSpec 60 0
      errResponse = responseLBS status400 [] . toLazyByteString . stringUtf8 . unpack
      defaultV2Settings = ThrottleV2.defaultThrottleSettings expirationInterval
      longPeriod = Throttle.defaultThrottleSettings { Throttle.throttlePeriod = 30000000 }
      shortPeriod = Throttle.defaultThrottleSettings { Throttle.throttlePeriod = 1 }
      longPeriodV2 = defaultV2Settings { ThrottleV2.throttleSettingsPeriod = 30000000 }
      shortPeriodV2 = defaultV2Settings { ThrottleV2.throttleSettingsPeriod = 1 }
      throttleV2 settings = ThrottleV2.newThrottle settings (over _Left errResponse . runExcept . Throttle.requestToKey)
  ipTh <- Throttle.initThrottler
  keyTh <- Throttle.initCustomThrottler :: IO (Throttle.CustomWaiThrottle Key)
  void $ serverWithThrottle Throttle.defaultThrottleSettings ipTh 3001
  void $ serverWithThrottle Throttle.defaultThrottleSettings keyTh 3002
  void $ serverWithThrottle longPeriod ipTh 3003
  void $ serverWithThrottle longPeriod keyTh 3004
  void $ serverWithThrottle shortPeriod ipTh 3005
  void $ serverWithThrottle shortPeriod keyTh 3006
  void . flip serverWithThrottleV2 3007 =<< (throttleV2 defaultV2Settings :: IO (ThrottleV2.Throttle Throttle.Address))
  void . flip serverWithThrottleV2 3008 =<< (throttleV2 defaultV2Settings :: IO (ThrottleV2.Throttle Key))
  void . flip serverWithThrottleV2 3009 =<< (throttleV2 longPeriodV2 :: IO (ThrottleV2.Throttle Throttle.Address))
  void . flip serverWithThrottleV2 3010 =<< (throttleV2 longPeriodV2 :: IO (ThrottleV2.Throttle Key))
  void . flip serverWithThrottleV2 3011 =<< (throttleV2 shortPeriodV2 :: IO (ThrottleV2.Throttle Throttle.Address))
  void . flip serverWithThrottleV2 3012 =<< (throttleV2 shortPeriodV2 :: IO (ThrottleV2.Throttle Key))

  putStrLn "benchmark"
  defaultMain [ benchmark "default by ip" 3001
              , benchmark "default by key" 3002
              , benchmark "long by ip" 3003
              , benchmark "long by key" 3004
              , benchmark "short by ip" 3005
              , benchmark "short by key" 3006
              , benchmark "default v2 by ip" 3007
              , benchmark "default v2 by key" 3008
              , benchmark "long v2 by ip" 3009
              , benchmark "long v2 by key" 3010
              , benchmark "short v2 by ip" 3011
              , benchmark "short v2 by key" 3012
              ]
