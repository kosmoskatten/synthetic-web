{-# LANGUAGE OverloadedStrings #-}
module SyntheticWeb.Observer
       ( service
       ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (force)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time ( UTCTime
                 , diffUTCTime
                 , getCurrentTime )
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
import SyntheticWeb.Counter (CounterSet)

service :: Int -> CounterSet -> IO ()
service port _ = do
  startTime <- getCurrentTime'
  let config = setPort port defaultConfig
  httpServe config $
    ifTop (renderStatistics startTime)
    <|> resourceNotFound

renderStatistics :: UTCTime -> Snap ()
renderStatistics startTime = do
  let response = setResponseCode 200 $
                 setContentType "text/html" emptyResponse
  putResponse response
  statsHeader <- renderStatsHeader startTime
  writeBS $ BS.unlines (htmlHead ++ statsHeader ++ htmlTail)

resourceNotFound :: Snap ()
resourceNotFound = do
  let response = setResponseCode 404 $
                 setContentType "text/plain" emptyResponse
  putResponse response
  writeBS "The requested resource was not found"

renderStatsHeader :: UTCTime -> Snap [BS.ByteString]
renderStatsHeader startTime = do
  now <- liftIO getCurrentTime'
  return [ "Up for " `BS.append` BS.pack (show (now `diffUTCTime` startTime))]  

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

getCurrentTime' :: IO UTCTime
getCurrentTime' = force <$> getCurrentTime

  
