{-# LANGUAGE OverloadedStrings #-}
module TestSuite.Server
    ( getTextPlainSize667
    , getTextHtmlSize5671
    , putTextPlain
    , postTextHtmlSize1234
    ) where

import Blaze.ByteString.Builder (Builder)
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
import System.IO.Streams (OutputStream)
import qualified System.IO.Streams as Streams
import Test.HUnit

type Url    = BS.ByteString
type Header = (BS.ByteString, BS.ByteString)
type Reply  = (StatusCode, Int, Maybe BS.ByteString)

-- | HTTP GET /667
-- Accept: text/plain. Shall result in 200 and a 667 bytes body.
getTextPlainSize667 :: Assertion
getTextPlainSize667 = 
  withServer $ do
    (respCode, respSize, contentType) <- "/667" `get` [("Accept", "text/plain")]
    200          @=? respCode
    667          @=? respSize
    "text/plain" @=? fromJust contentType

-- | HTTP GET /5671
-- Accept: text/html. Shall result in 200 and 5671 bytes body.
getTextHtmlSize5671 :: Assertion
getTextHtmlSize5671 =
  withServer $ do
    (respCode, respSize, contentType) <- "/5671" `get` [("Accept", "text/html")]
    200         @=? respCode
    5671        @=? respSize
    "text/html" @=? fromJust contentType

-- | HTTP PUT /123
-- Content-Type: text/plain. Shall result in 204 and no body.
putTextPlain :: Assertion
putTextPlain =
  withServer $ do
    (respCode, respSize, contentType) <- 
        "/123" `put` [("Content-Type", "text/plain")]
    204     @=? respCode
    0       @=? respSize
    Nothing @=? contentType

-- | HTTP POST /1234
-- Accept: text/html, Content-Type: text/html. Shall result in 201 and
-- a 1234 bytes body.
postTextHtmlSize1234 :: Assertion
postTextHtmlSize1234 =
  withServer $ do
    (respCode, respSize, contentType) <- 
        "/1234" `post` [("Accept", "text/html"), ("Content-Type", "text/html")]
    201         @=? respCode
    1234        @=? respSize
    "text/html" @=? fromJust contentType

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

get :: Url -> [Header] -> IO Reply
get = op GET emptyBody

put :: Url -> [Header] -> IO Reply
put = op PUT (Streams.write (Just "Just some bytes"))

post :: Url -> [Header] -> IO Reply
post = op POST (Streams.write (Just "Just some bytes"))

op :: Method -> (OutputStream Builder -> IO ()) -> Url -> [Header] -> IO Reply
op method output url headers =
  withConnection (openConnection serviceName
                   (fromIntegral servicePort)) $ \conn -> do
    let request = buildRequest $ do
                    http method url
                    mapM_ (uncurry setHeader) headers
    sendRequest conn request output
    receiveResponse conn $ \resp inp -> do
      let respCode = getStatusCode resp
      respSize <- BS.length <$> concatHandler resp inp
      return (respCode, respSize, getHeader resp "Content-Type")
