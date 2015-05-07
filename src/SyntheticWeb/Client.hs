module SyntheticWeb.Client
       ( service
       ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Monad (forever, replicateM, void)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import SyntheticWeb.Counter (CounterPair)
import SyntheticWeb.Host (Host (..))
import SyntheticWeb.Plan (Plan, Pattern (..))
import SyntheticWeb.Task (Task, patternFrom)
import System.Random (randomRIO)

type TaskSelector = IO Task

service :: Host -> Int -> Vector Task -> IO ()
service host numWorkers tasks = do
  let taskSelector = mkTaskSelector tasks
  void $ waitAnyCancel =<<
    replicateM numWorkers (async $ worker taskSelector)

mkTaskSelector :: Vector Task -> TaskSelector
mkTaskSelector tasks =
  let len = Vector.length tasks
  in Vector.unsafeIndex tasks <$> randomRIO (0, len - 1)

worker :: TaskSelector -> IO ()
worker select =
  forever $ do
    task <- select
    print $ patternFrom task
    threadDelay 1000000
