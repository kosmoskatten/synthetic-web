{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module SyntheticWeb.Observer
       ( service
       ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
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
import SyntheticWeb.Counter ( ByteCounter (..)
                            , CounterSet
                            , FrozenSet (..)
                            , GlobalCounter (..)
                            , atomically
                            , freeze )
import Text.Printf (printf)

service :: Int -> CounterSet -> IO ()
service port counterSet = do
  let config = setPort port defaultConfig
  httpServe config $
    ifTop (renderStatistics counterSet)
    <|> resourceNotFound

renderStatistics :: CounterSet -> Snap ()
renderStatistics counterSet = do
  frozenSet <- liftIO (atomically $ freeze counterSet)
  let response = setResponseCode 200 $
                 setContentType "text/html" emptyResponse
  putResponse response
  writeBS htmlHead
  writeBS $ globalStats frozenSet
  writeBS htmlFoot

resourceNotFound :: Snap ()
resourceNotFound = do
  let response = setResponseCode 404 $
                 setContentType "text/plain" emptyResponse
  putResponse response
  writeBS "The requested resource was not found"

globalStats :: FrozenSet -> BS.ByteString
globalStats (FrozenSet (GlobalCounter {..}, _)) = BS.unlines
  [ "============================================================<br>"
  , BS.pack $ printf " Total pattern time: %.2fs<br>"
                     ((realToFrac totalPatternTime) :: Double)
  , BS.pack $ printf " Total downloaded bytes: %ld<br>" $ 
                     download totalByteCount
  ]

htmlHead :: BS.ByteString
htmlHead = BS.unlines
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

htmlFoot :: BS.ByteString
htmlFoot = BS.unlines
  [ "</body>"
  , "</html>"
  ]
