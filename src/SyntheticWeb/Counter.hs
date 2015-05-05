module SyntheticWeb.Counter
       ( ByteCounter (..)
       , GlobalCounter (..)
       , PatternCounter (..)
       , Throughput (..)
       , CounterPair (..)
       , CounterSet (..)
       , incDownload
       , incUpload
       , mkGlobalCounter
       , mkPatternCounter
       , toThroughput
       ) where

import Control.Concurrent.STM (TVar)
import SyntheticWeb.Counter.ByteCounter
import SyntheticWeb.Counter.GlobalCounter
import SyntheticWeb.Counter.PatternCounter
import SyntheticWeb.Counter.Throughput

-- | A pair of PatternCounter and GlobalCounter. A such pair is to be
-- assigned to a worker for pattern execution.
newtype CounterPair = CounterPair (TVar PatternCounter, TVar GlobalCounter)

-- | A set of the GlobalCounter and all the PatternCounters.
newtype CounterSet = CounterSet (TVar GlobalCounter, [TVar PatternCounter])
