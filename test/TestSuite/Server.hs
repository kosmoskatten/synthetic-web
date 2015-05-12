{-# LANGUAGE OverloadedStrings #-}
module TestSuite.Server
    ( getTextPlainSize667
    , getTextHtmlSize5671
    , putTextPlain
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Network.Http.Client ( Method (..)
                           , StatusCode
                           , buildRequest
                           , emptyBody
                           , http
                           , concatHandler
                           , getHeader
                           , getStatusCode
                           , openConnection
                           , receiveResponse
                           , setHeader
                           , sendRequest
                           , withConnection )
import SyntheticWeb.Server (service)
import qualified System.IO.Streams as Streams
import Test.HUnit

-- | HTTP GET /667
-- Accept: text/plain. Shall result in 200 OK and 667 bytes body.
getTextPlainSize667 :: Assertion
getTextPlainSize667 = 
  withServer $ do
    (respCode, respSize, contentType) <- "/667" `get` [("Accept", "text/plain")]
    200          @=? respCode
    667          @=? respSize
    "text/plain" @=? fromJust contentType

-- | HTTP GET /5671
-- Accept: text/html. Shall result in 200 OK and 5671 bytes body.
getTextHtmlSize5671 :: Assertion
getTextHtmlSize5671 =
  withServer $ do
    (respCode, respSize, contentType) <- "/5671" `get` [("Accept", "text/html")]
    200         @=? respCode
    5671        @=? respSize
    "text/html" @=? fromJust contentType

putTextPlain :: Assertion
putTextPlain =
  withServer $ do
    (respCode, respSize, contentType) <- "/123" `put` [("Accept", "text/plain")]
    204     @=? respCode
    0       @=? respSize
    Nothing @=? contentType

serviceName :: BS.ByteString
serviceName = "127.0.0.1"

servicePort :: Int
servicePort = 22000

withServer :: Assertion -> IO ()
withServer act = 
  bracket (async $ service servicePort) 
          cancel $ \_ -> do
            threadDelay 1000000
            act

get :: BS.ByteString 
      -> [(BS.ByteString, BS.ByteString)] 
      -> IO (StatusCode, Int, Maybe BS.ByteString)
get url reqHeaders =
  withConnection (openConnection serviceName 
                  (fromIntegral servicePort)) $ \conn -> do
    let request = 
            buildRequest $ do
              http GET url
              mapM_ (uncurry setHeader) reqHeaders
    sendRequest conn request emptyBody
    receiveResponse conn $ \resp inp -> do
      let respCode = getStatusCode resp
      respSize <- BS.length <$> concatHandler resp inp
      return (respCode, respSize, getHeader resp "Content-Type")

put :: BS.ByteString
       -> [(BS.ByteString, BS.ByteString)]
       -> IO (StatusCode, Int, Maybe BS.ByteString)
put url reqHeaders =
  withConnection (openConnection serviceName
                  (fromIntegral servicePort)) $ \conn -> do
    let request = 
            buildRequest $ do
              http PUT url
              mapM_ (uncurry setHeader) reqHeaders
    sendRequest conn request $ Streams.write (Just "Just some bytes")
    receiveResponse conn $ \resp inp -> do
      let respCode = getStatusCode resp
      respSize <- BS.length <$> concatHandler resp inp
      return (respCode, respSize, getHeader resp "Content-Type")
