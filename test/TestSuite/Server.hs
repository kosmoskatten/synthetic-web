{-# LANGUAGE OverloadedStrings #-}
module TestSuite.Server
    ( getTextPlainSize667
    , getTextHtmlSize5671
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Network.Http.Client
import SyntheticWeb.Server (service)
import Test.HUnit

getTextPlainSize667 :: Assertion
getTextPlainSize667 = 
    withServer $ do
      (respCode, respSize, contentType) <- "/667" <@ [("Accept", "text/plain")]
      200          @=? respCode
      667          @=? respSize
      "text/plain" @=? fromJust contentType

getTextHtmlSize5671 :: Assertion
getTextHtmlSize5671 =
    withServer $ do
      (respCode, respSize, contentType) <- "/5671" <@ [("Accept", "text/html")]
      200         @=? respCode
      5671        @=? respSize
      "text/html" @=? fromJust contentType

serviceName :: BS.ByteString
serviceName = "localhost"

servicePort :: Int
servicePort = 22000

withServer :: Assertion -> IO ()
withServer act = 
    bracket (async $ service servicePort) 
            cancel $ \_ -> do
              threadDelay 1000000
              act

(<@) :: BS.ByteString 
      -> [(BS.ByteString, BS.ByteString)] 
      -> IO (StatusCode, Int, Maybe BS.ByteString)
url <@ reqHeaders =
    withConnection (openConnection serviceName 
                    (fromIntegral servicePort)) $ \conn -> do
      request <- buildRequest $ do
                   http GET url
                   mapM_ (uncurry setHeader) reqHeaders
      sendRequest conn request emptyBody
      receiveResponse conn $ \resp inp -> do
        let respCode = getStatusCode resp
        respSize <- BS.length <$> concatHandler resp inp
        return (respCode, respSize, getHeader resp "Content-Type")
