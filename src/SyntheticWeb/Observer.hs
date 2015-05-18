{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module SyntheticWeb.Observer
       ( service
       ) where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Time (NominalDiffTime)
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
                            , PatternCounter (..)
                            , freeze 
                            , toThroughput )
import Text.Printf (printf)

service :: Int -> CounterSet -> IO ()
service port counterSet = do
  let config = setPort port defaultConfig
  httpServe config $
    ifTop (renderStatistics counterSet)
    <|> resourceNotFound

renderStatistics :: CounterSet -> Snap ()
renderStatistics counterSet = do
  frozenSet@(FrozenSet (_, _, patternCounters)) <- liftIO $ freeze counterSet
  let response = setResponseCode 200 $
                 setContentType "text/html" emptyResponse
  putResponse response
  writeBS htmlHead
  writeBS $ htmlDiv "pink" (globalStats frozenSet)
  forM_ (zip patternCounters alterStyles) $ \(patternCounter, style) ->
    writeBS $ htmlDiv style (patternStats frozenSet patternCounter)
  writeBS htmlFoot

resourceNotFound :: Snap ()
resourceNotFound = do
  let response = setResponseCode 404 $
                 setContentType "text/plain" emptyResponse
  putResponse response
  writeBS "The requested resource was not found"

htmlDiv :: String -> [BS.ByteString] -> BS.ByteString
htmlDiv style xs =
    BS.unlines $ concat
          [ [ BS.pack $ printf "<div id=\"%s\">" style
            , "<pre style=\"font-family:Courier,monospace;\">" ]
          , xs
          , [ "</pre>"
            , "</div>" ] ]

globalStats :: FrozenSet -> [BS.ByteString]
globalStats (FrozenSet (t, GlobalCounter {..}, _)) = 
    let (dlTpt, ulTpt) = toThroughput totalByteCount t
    in [ "==========================================================="
       , BS.pack $ printf " Uptime              : %.2fs" 
                          (timeToDouble t)
       , BS.pack $ printf " Total pattern time  : %.2fs"
                          (timeToDouble totalPatternTime)
       , BS.pack $ printf " Total download      : %ld bytes at %s"
                          (download totalByteCount) (show dlTpt)
       , BS.pack $ printf " Total upload        : %ld bytes at %s"
                          (upload totalByteCount) (show ulTpt)
       , BS.pack $ printf " Total # activations : %ld" totalActivations
       , BS.pack $ printf " Total sleep time    : %.2fs (%.2f%% of total)"
                          (timeToDouble totalSleepTime)
                          (timeToDouble $ 
                            totalSleepTime `percentOf` totalPatternTime)
       , BS.pack $ printf " Total latency time  : %.2fs (%.2f%% of total)"
                          (timeToDouble totalLatencyTime)
                          (timeToDouble $ 
                            totalLatencyTime `percentOf` totalPatternTime)
       ]

patternStats :: FrozenSet -> PatternCounter -> [BS.ByteString]
patternStats (FrozenSet (t, GlobalCounter {..}, _)) PatternCounter {..} =
    let (dlTpt, ulTpt) = toThroughput byteCount t
    in [ "==========================================================="
       , BS.pack $ printf " Name                : %s" patternName
       , BS.pack $ printf " Pattern time        : %.2fs (%.2f%% of total)"
                          (timeToDouble patternTime)
                          (timeToDouble $
                            patternTime `percentOf` totalPatternTime)
       , BS.pack $ printf " Pattern download    : %ld bytes at %s"
                          (download byteCount) (show dlTpt)
       , BS.pack $ printf " Pattern upload      : %ld bytes at %s"
                          (upload byteCount) (show ulTpt)
       , BS.pack $ printf " # activations       : %ld (%.2f%% of total)"
                          activations 
                          ((fromIntegral activations `percentOf` 
                            fromIntegral totalActivations) :: Double)
       , BS.pack $ printf " Sleep time          : %.2fs (%.2f%% of pattern)"
                          (timeToDouble sleepTime)
                          (timeToDouble $ sleepTime `percentOf` patternTime)
       , BS.pack $ printf " Latency time        : %.2fs (%.2f%% of pattern)"
                          (timeToDouble latencyTime)
                          (timeToDouble $ latencyTime `percentOf` patternTime)

       ]
    
htmlHead :: BS.ByteString
htmlHead = BS.unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<title>Synthetic Web Statistics</title>"
  , "<meta http-equiv=\"refresh\" content=\"1\"/>"
  , "<style>"
  , "#pink{background-color:pink;margin-top:-17px;}"
  , "#blue{background-color:lightblue;margin-top:-17px;}"
  , "#gray{background-color:lightgray;margin-top:-17px;}"
  , "</style>"
  , "</head>"
  , "<body>"
  ]

htmlFoot :: BS.ByteString
htmlFoot = BS.unlines
  [ "</body>"
  , "</html>"
  ]

percentOf :: (Fractional a, Ord a) => a -> a -> a
percentOf t1 t2 
    | t2 > 0    = t1 / t2 * 100
    | otherwise = 0

timeToDouble :: NominalDiffTime -> Double
timeToDouble = realToFrac

alterStyles :: [String]
alterStyles = concat $ repeat ["gray", "blue"]
