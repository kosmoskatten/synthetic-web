module SyntheticWeb.Client
       ( service
       ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Monad (forever, replicateM, void)
import qualified Data.Vector as V
import SyntheticWeb.Plan (Plan, Pattern (..), expand)
import System.Random (randomRIO)

type PatternSelector = IO Pattern

service :: Int -> Plan -> IO ()
service workers plan = do
  let patterns        = expand plan
      numPatterns     = V.length patterns
      patternSelector = do
        index <- randomRIO (0, numPatterns - 1)
        return $ V.unsafeIndex patterns  index
  void $ waitAnyCancel =<<
    (replicateM workers $ async (worker patternSelector))

worker :: PatternSelector -> IO ()
worker patternSelector =
  forever $ do
    pattern <- patternSelector
    putStrLn $ show pattern
    threadDelay 1000000
