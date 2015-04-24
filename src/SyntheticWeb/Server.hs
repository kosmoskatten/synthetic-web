{-# LANGUAGE OverloadedStrings #-}
module SyntheticWeb.Server
       ( service
       ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (deepseq)
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

service :: Int -> IO ()
service port = do
  let config =  setPort port $
                setCompression False defaultConfig
      payload = randomData
  preheatPayload payload
  httpServe config $ route
                [ (":resource", method GET $ do
                     getReplyHandler
                     generatePayload payload )
                , (":resource", method PUT putReplyHandler)
                ] <|> resourceNotFoundHandler

getReplyHandler :: Snap ()
getReplyHandler = do
  accept <- getHeader "Accept" <$> getRequest
  let contentType = 
          maybe emptyResponse (`setContentType` emptyResponse) accept
  putResponse $ setResponseCode 200 contentType

putReplyHandler :: Snap ()
putReplyHandler = putResponse $ setResponseCode 204 emptyResponse

generatePayload :: LBS.ByteString -> Snap ()
generatePayload payload = do
  reqSize <- read . BS.unpack . fromJust <$> getParam "resource"
  writeLBS $ LBS.take reqSize payload

resourceNotFoundHandler :: Snap ()
resourceNotFoundHandler = do
  let response = setResponseCode 404 $
                 setContentType "text/plain" emptyResponse
  putResponse response
  writeBS "The requested resource was not found"

preheatPayload :: LBS.ByteString -> IO ()
preheatPayload payload = return $ LBS.take 20000000 payload `deepseq` ()
