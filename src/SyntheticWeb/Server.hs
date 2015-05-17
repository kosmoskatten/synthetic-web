{-# LANGUAGE OverloadedStrings #-}
module SyntheticWeb.Server
       ( service
       ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Snap.Core ( Snap
                 , Method (..)
                 , getHeader
                 , getRequest
                 , emptyResponse
                 , getParam
                 , method
                 , putResponse
                 , route
                 , setResponseCode
                 , setContentType
                 , writeLBS
                 , writeBS )
import Snap.Http.Server ( defaultConfig
                        , httpServe
                        , setPort
                        , setCompression )
import SyntheticWeb.RandomData (randomData)

-- | Initialize the service and route requests.
service :: Int -> IO ()
service port = do
  let config =  setPort port $
                setCompression False defaultConfig
      payload = randomData
  httpServe config $ route
                [ ( ":resource"
                  , method GET $ do
                      contentReplyHeader 200
                      generate payload )
                , ( ":resource"
                  , method POST $ do
                      contentReplyHeader 201
                      generate payload )
                , ( ":resource"
                  , method PUT $ noContentReplyHeader 204 )
                ] <|> resourceNotFoundHandler

-- | Make a reply header which is using the incoming accept header as
-- content.
contentReplyHeader :: Int -> Snap ()
contentReplyHeader code = do
  maybeAccept <- getHeader "Accept" <$> getRequest
  let contentType =
          maybe emptyResponse (`setContentType` emptyResponse) maybeAccept
  putResponse $ setResponseCode code contentType

-- | Make a reply header for no content, only response code.
noContentReplyHeader :: Int -> Snap ()
noContentReplyHeader code = putResponse $ setResponseCode code emptyResponse

-- | Generate the amount of payload specified by the url.
generate :: LBS.ByteString -> Snap ()
generate payload = do
  reqSize <- read . BS.unpack . fromJust <$> getParam "resource"
  writeLBS $ LBS.take reqSize payload

resourceNotFoundHandler :: Snap ()
resourceNotFoundHandler = do
  let response = setResponseCode 404 $
                 setContentType "text/plain" emptyResponse
  putResponse response
  writeBS "The requested resource was not found"
