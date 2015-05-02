{-# LANGUAGE OverloadedStrings #-}
module SyntheticWeb.Observer
       ( service
       ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as BS
import Snap.Core ( Snap
                 , ifTop
                 , emptyResponse
                 , putResponse
                 , setContentType
                 , setResponseCode
                 , writeBS )
import Snap.Http.Server ( defaultConfig
                        , httpServe
                        , setPort )

service :: Int -> IO ()
service port = do
  let config = setPort port defaultConfig
  httpServe config $
    ifTop renderStatistics
    <|> resourceNotFound

renderStatistics :: Snap ()
renderStatistics = do
  let response = setResponseCode 200 $
                 setContentType "text/html" emptyResponse
  putResponse response
  writeBS $ BS.unlines (htmlHead ++ foo ++ htmlTail)

resourceNotFound :: Snap ()
resourceNotFound = do
  let response = setResponseCode 404 $
                 setContentType "text/plain" emptyResponse
  putResponse response
  writeBS "The requested resource was not found"

foo :: [BS.ByteString]
foo = [ "<p class=\"mono\">Hepp hepp</p>" ]

htmlHead :: [BS.ByteString]
htmlHead =
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<title>Synthetic Web Statistics</title>"
  , "<meta http-equiv=\"refresh\" content=\"1\"/>"
  , "<style>"
  , "p.mono{"
  , "font-family:Courier,monospace;"
  , "}"
  , "</style>"
  , "</head>"
  , "<body>"
  ]

htmlTail :: [BS.ByteString]
htmlTail =
  [ "</body>"
  , "</html>"
  ]

  
