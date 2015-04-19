{-# LANGUAGE OverloadedStrings #-}
module SyntheticWeb.Server
       ( service
       ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as BS
import Snap.Core ( Snap
                 , emptyResponse
                 , putResponse
                 , setResponseCode
                 , setContentType
                 , writeBS )
import Snap.Http.Server ( defaultConfig
                        , httpServe
                        , setPort
                        , setCompression )

service :: Int -> IO ()
service port = do
  let config = setPort port $
               setCompression False defaultConfig
  httpServe config resourceNotFoundHandler

resourceNotFoundHandler :: Snap ()
resourceNotFoundHandler = do
  let response = setResponseCode 404 $
                 setContentType "text/plain" emptyResponse
  putResponse response
  writeBS "The requested resource was not found"

