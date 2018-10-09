{-
simple benchmarking tool:
stack install
benchmark -o bench.html (or benchmark --json bench.json)
-}
import Prelude

import Control.Concurrent (ThreadId, forkIO)
import Control.Lens ((&), (.~), (?~))
import Control.Monad ((>=>), void)
import Criterion (Benchmark, bench, bgroup, whnfIO)
import Criterion.Main (defaultMain)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Hashable (Hashable)
import Data.Foldable (find)
import Data.Monoid ((<>))
import Data.Text (Text, strip, stripPrefix, toCaseFold)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (hAuthorization, status200, status400)
import Network.Wai (Request, Response, requestBody, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Middleware.Throttle as Throttle
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

extractKey :: Request -> Either Response Key
extractKey =
  let authorization = find ((== hAuthorization) . fst) . requestHeaders
        >=> stripPrefix "basic" . toCaseFold . decodeUtf8 . snd
        >=> Just . Key . strip
  in maybe (Left $ responseLBS status400 [] "No authorization header") Right . authorization

serverWithThrottle :: (Eq a, Hashable a) => Throttle.Throttle a -> Int -> IO ThreadId
serverWithThrottle th port =
  let app = Throttle.throttle th $ \ x f ->
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
      defaultSettings = Throttle.defaultThrottleSettings expirationInterval
      longPeriod = defaultSettings { Throttle.throttleSettingsPeriod = 30000000 }
      shortPeriod = defaultSettings { Throttle.throttleSettingsPeriod = 1 }
      ipThrottle settings = Throttle.initThrottler settings
      keyThrottle settings = Throttle.initCustomThrottler settings extractKey
  void . flip serverWithThrottle 3007 =<< ipThrottle defaultSettings
  void . flip serverWithThrottle 3008 =<< keyThrottle defaultSettings
  void . flip serverWithThrottle 3009 =<< ipThrottle longPeriod
  void . flip serverWithThrottle 3010 =<< keyThrottle longPeriod
  void . flip serverWithThrottle 3011 =<< ipThrottle shortPeriod
  void . flip serverWithThrottle 3012 =<< keyThrottle shortPeriod

  putStrLn "benchmark"
  defaultMain [ benchmark "default by ip" 3007
              , benchmark "default by key" 3008
              , benchmark "long by ip" 3009
              , benchmark "long by key" 3010
              , benchmark "short by ip" 3011
              , benchmark "short by key" 3012
              ]
