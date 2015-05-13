module SyntheticWeb.Client.Executor
    ( executeTask
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import SyntheticWeb.Client.TimedAction (timedAction)
import SyntheticWeb.Counter (CounterPair, updateSleepTime)
import SyntheticWeb.Task (Task, counterFrom, patternFrom)
import SyntheticWeb.Plan.Types ( Activity (..)
                               , Duration (..)
                               , Pattern (..) )

-- | Execute one task.
executeTask :: Task -> IO ()
executeTask task = do
  let counter = counterFrom task
      pattern = patternFrom task
  mapM_ (executeActivity counter) $ activities pattern

-- | Execute one client activity. Counters related to the activity -
-- timings and byte counters - shall be updated.
executeActivity :: CounterPair -> Activity -> IO ()

-- | Delay the task worker thread for the specified duration.
executeActivity counter (SLEEP duration) = do
  ((), timeItTook) <- timedAction (threadDelay $ toDelay duration)
  atomically $ updateSleepTime timeItTook counter
    where
      toDelay :: Duration -> Int
      toDelay (Usec t) = t
      toDelay (Msec t) = t * 1000
      toDelay (Sec t)  = t * 1000000
executeActivity _ (GET _ _ _) = return ()
executeActivity _ (PUT _ _) = return ()
executeActivity _ (POST _ _ _ _) = return ()
