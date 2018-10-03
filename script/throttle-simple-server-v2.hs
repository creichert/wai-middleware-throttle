{-
simple server for profiling:
stack build --profile
stack exec -- throttle-simple-server-v2 +RTS -h
hp2ps throttle-simple-server-v2.hp # open in preview
-}
import Prelude

import Control.Concurrent (ThreadId, forkIO, newQSemN, signalQSemN, threadDelay, waitQSemN)
import Control.Exception (finally)
import Control.Lens ((&), (.~), (?~), over, _Left)
import Control.Monad ((>=>), replicateM_, void)
import Control.Monad.Except (throwError, runExcept)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
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
import System.Random (randomIO)

data Method = Get | Post

newtype Key = Key Text
  deriving (Eq, Ord, Hashable)

instance Throttle.RequestHashable Key where
  requestToKey =
    let authorization = find ((== hAuthorization) . fst) . requestHeaders
          >=> stripPrefix "basic" . toCaseFold . decodeUtf8 . snd
          >=> Just . Key . strip
    in maybe (throwError "No authorization header") pure . authorization

serverWithThrottleV2 :: (Eq a, Hashable a) => ThrottleV2.Throttle a -> Int -> IO ThreadId
serverWithThrottleV2 th port =
  let app = ThrottleV2.runThrottle th $ \ x f ->
        f . responseLBS status200 [] . fromStrict =<< requestBody x
  in forkIO $ run port app

makeRequest :: [String] -> Int -> Method -> IO ()
makeRequest identifiers port method = do
  index <- flip mod (length identifiers) <$> randomIO
  let identifier = identifiers !! index
      endpoint = "http://localhost:" <> show port <> "/"
      options = defaults &
        header hAuthorization .~ ["BASIC " <> C8.pack identifier] &
#if !MIN_VERSION_wreq(0,5,0)
        checkStatus ?~ (\ _ _ _ -> Nothing)
#else
        checkResponse ?~ (\ _ _ -> pure ())
#endif
      doPost = postWith options endpoint ("foo" :: ByteString)
      doGet = getWith options endpoint
  case method of
    Get -> void doGet
    Post -> void doPost

baseIdentifiers :: [String]
baseIdentifiers =
  [ "foo"
  , "bar"
  , "baz"
  , "bin"
  ]

main :: IO ()
main = do
  let expirationInterval = TimeSpec 60 0
      errResponse = responseLBS status400 [] . toLazyByteString . stringUtf8 . unpack
      defaultV2Settings = ThrottleV2.defaultThrottleSettings expirationInterval
  th :: ThrottleV2.Throttle Key <- ThrottleV2.newThrottle defaultV2Settings (over _Left errResponse . runExcept . Throttle.requestToKey)
  qs <- newQSemN 10
  waitQSemN qs 10
  void $ serverWithThrottleV2 th 3000
  replicateM_ 10 $ void . forkIO . flip finally (signalQSemN qs 1) $
    replicateM_ 1000 $ makeRequest baseIdentifiers 3000 Get >> makeRequest baseIdentifiers 3000 Post >> threadDelay 1000000
  waitQSemN qs 10
