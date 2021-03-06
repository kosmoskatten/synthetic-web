{-# LANGUAGE TupleSections #-}
module SyntheticWeb.Counter
       ( ByteCounter (..)
       , GlobalCounter (..)
       , PatternCounter (..)
       , CounterPair (..)
       , CounterSet (..)
       , FrozenSet (..)
       , mkCounterSet
       , mkGlobalCounter
       , mkPatternCounter
       , activatePattern
       , updateByteCount
       , updatePatternTime
       , updateSleepTime
       , updateLatencyTime
       , freeze
       , toThroughput
       , atomically
       ) where

import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, readTVar)
import Control.DeepSeq (force)
import Data.Time ( UTCTime
                 , NominalDiffTime
                 , getCurrentTime
                 , diffUTCTime )
import SyntheticWeb.Counter.ByteCounter
import SyntheticWeb.Counter.GlobalCounter
import SyntheticWeb.Counter.PatternCounter
import SyntheticWeb.Counter.Throughput

-- | A pair of PatternCounter and GlobalCounter. A such pair is to be
-- assigned to a worker for pattern execution.
newtype CounterPair = CounterPair (TVar PatternCounter, TVar GlobalCounter)

-- | A set of the GlobalCounter and all the PatternCounters.
newtype CounterSet = 
    CounterSet (UTCTime, TVar GlobalCounter, [TVar PatternCounter])

-- | A frozen set of Global counter and all the PatternCounters.
newtype FrozenSet = FrozenSet (NominalDiffTime, GlobalCounter, [PatternCounter])

-- | Make a time stamped counter set.
mkCounterSet :: TVar GlobalCounter -> [TVar PatternCounter] -> IO CounterSet
mkCounterSet g ps = do
  t <- getCurrentTime'
  return $ CounterSet (t, g, ps)

-- | Activate a pattern. It will increase the pattern local
-- activations counter and the global total activations counter.
activatePattern :: CounterPair -> STM ()
activatePattern (CounterPair (p, g)) = do
  modifyTVar p $ \p' -> p' { activations = activations p' + 1 }
  modifyTVar g $ \g' -> g' { totalActivations = totalActivations g' + 1 }

-- | Update the byte count. It will update the pattern local byte
-- counter as well as the global byte counter.
updateByteCount :: ByteCounter -> CounterPair -> STM ()
updateByteCount bytes (CounterPair (p, g)) = do
  modifyTVar p $ \p' -> p' { byteCount = byteCount p' `addByteCount` bytes }
  modifyTVar g $ \g' ->
    g' { totalByteCount = totalByteCount g' `addByteCount` bytes }

-- | Update the pattern time. It will update the pattern local time
-- counter as well as the global pattern time counter.
updatePatternTime :: NominalDiffTime -> CounterPair -> STM ()
updatePatternTime delta (CounterPair (p, g)) = do
  modifyTVar p $ \p' -> p' { patternTime = patternTime p' + delta }
  modifyTVar g $ \g' -> g' { totalPatternTime = totalPatternTime g' + delta }

-- | Update the sleep time. It will update the pattern local time
-- counter as well as the global sleep time counter.
updateSleepTime :: NominalDiffTime -> CounterPair -> STM ()
updateSleepTime delta (CounterPair (p, g)) = do
  modifyTVar p $ \p' -> p' { sleepTime = sleepTime p' + delta }
  modifyTVar g $ \g' -> g' { totalSleepTime = totalSleepTime g' + delta }

-- | Update the latency time. It will update the pattern local time
-- counter as well as the global latency time counter.
updateLatencyTime :: NominalDiffTime -> CounterPair -> STM ()
updateLatencyTime delta (CounterPair (p, g)) = do
  modifyTVar p $ \p' -> p' { latencyTime = latencyTime p' + delta }
  modifyTVar g $ \g' -> g' { totalLatencyTime = totalLatencyTime g' + delta }

-- | Freeze a snapshot of the counters from TVar to pure values and
-- calculate the difftime from the start.
freeze :: CounterSet -> IO FrozenSet
freeze (CounterSet (t, g, ps)) = do
  t' <- getCurrentTime'
  atomically $ do
    g'  <- readTVar g
    ps' <- mapM readTVar ps
    return $! FrozenSet (t' `diffUTCTime` t, g', ps')

getCurrentTime' :: IO UTCTime
getCurrentTime' = force <$> getCurrentTime


