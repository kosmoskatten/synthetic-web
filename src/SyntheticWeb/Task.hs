-- | The Task provides the building of tasks from a Plan. A task is a
-- pattern combined with its counters.
module SyntheticWeb.Task 
    ( Task (..)
    , mkTaskSet
    ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Monad (foldM)
import Data.Vector (Vector, fromList)
import SyntheticWeb.Counter ( CounterPair (..)
                            , CounterSet (..)
                            , GlobalCounter
                            , PatternCounter (patternName)
                            , mkGlobalCounter
                            , mkPatternCounter )
import SyntheticWeb.Plan ( Plan (..)
                         , Pattern (..)
                         , Weight (..) )

-- | A pair of a Pattern with its counters.
newtype Task = Task (Pattern, CounterPair)

-- | Build a pair of a CounterSet and a vector of Tasks.
mkTaskSet :: Plan -> IO (CounterSet, Vector Task)
mkTaskSet plan = do
  globalCounter   <- newTVarIO mkGlobalCounter
  taskList        <- mkTaskList globalCounter plan
  patternCounters <- uniquePatternCounters taskList
  return (CounterSet (globalCounter, patternCounters), fromList taskList)

mkTaskList :: TVar GlobalCounter -> Plan -> IO [Task]
mkTaskList global (Plan plan) = concat <$> mapM go plan
    where
      go :: (Weight, Pattern) -> IO [Task]
      go (Weight w, pattern) = do
        counter <- newTVarIO $ mkPatternCounter (name pattern)
        return $ replicate w (Task (pattern, CounterPair (counter, global)))

uniquePatternCounters :: [Task] -> IO [TVar PatternCounter]
uniquePatternCounters tasks = reverse <$> foldM examine [] tasks
    where
      examine :: [TVar PatternCounter] -> Task -> IO [TVar PatternCounter]
      examine acc (Task (pattern, pair)) = do
        let CounterPair pair' = pair
        inList <- inTaskList (name pattern) acc
        return (if inList then fst pair':acc else return acc)

inTaskList :: String -> [TVar PatternCounter] -> IO Bool
inTaskList _ [] = return False
inTaskList expected (x:xs) = do
  patternName' <- patternName <$> readTVarIO x
  if expected == patternName' then
      return True else inTaskList expected xs
