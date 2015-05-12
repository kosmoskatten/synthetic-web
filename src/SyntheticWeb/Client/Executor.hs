module SyntheticWeb.Client.Executor
    ( executeTask
    ) where

import SyntheticWeb.Counter (CounterPair)
import SyntheticWeb.Task (Task, counterFrom, patternFrom)
import SyntheticWeb.Plan.Types (Activity (..), Pattern (..))

-- | Execute one task.
executeTask :: Task -> IO ()
executeTask task = do
  let counter = counterFrom task
      pattern = patternFrom task
  mapM_ (executeActivity counter) $ activities pattern

executeActivity :: CounterPair -> Activity -> IO ()
executeActivity _ (SLEEP duration) = return ()
executeActivity _ (GET _ _ _) = return ()
executeActivity _ (PUT _ _) = return ()
executeActivity _ (POST _ _ _ _) = return ()
