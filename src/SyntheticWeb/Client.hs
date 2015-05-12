module SyntheticWeb.Client
       ( service
       ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Concurrent.STM (atomically)
import Control.Monad (forever, replicateM, void)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import SyntheticWeb.Client.Executor (executeTask)
import SyntheticWeb.Client.TimedAction (timedAction)
import SyntheticWeb.Counter (activatePattern, updatePatternTime)
import SyntheticWeb.Host (Host (..))
import SyntheticWeb.Plan (Plan, Pattern (..))
import SyntheticWeb.Task (Task, counterFrom)
import System.Random (randomRIO)

type TaskSelector = IO Task

-- | Initialize the client service.
service :: Host -> Int -> Vector Task -> IO ()
service host numWorkers tasks = do
  let taskSelector = mkTaskSelector tasks
  void $ waitAnyCancel =<<
    replicateM numWorkers (async $ worker taskSelector)

-- | Make a function that randomly (uniformly distributed) will select
-- a task from the vector of tasks.
mkTaskSelector :: Vector Task -> TaskSelector
mkTaskSelector tasks =
  let len = Vector.length tasks
  in Vector.unsafeIndex tasks <$> randomRIO (0, len - 1)

-- | Worker thread. Forever it will select and execute tasks.
worker :: TaskSelector -> IO ()
worker select =
  forever $ do
    task <- select
    atomically (activatePattern $ counterFrom task)
    ((), timeOfTask) <- timedAction $ executeTask task
    atomically (updatePatternTime timeOfTask $ counterFrom task)
    
