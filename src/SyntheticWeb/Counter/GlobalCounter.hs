module SyntheticWeb.Counter.GlobalCounter
    ( GlobalCounter (..)
    , mkGlobalCounter
    ) where

import Data.Time (NominalDiffTime)
import SyntheticWeb.Counter.ByteCounter (ByteCounter, empty)
import GHC.Int (Int64)

data GlobalCounter =
    GlobalCounter { totalByteCount   :: !ByteCounter
                    -- ^ The total number of download and upload bytes
                    -- for all executed patterns.
                  , totalActivations :: {-# UNPACK #-} !Int64
                    -- ^ The total number of activations for all patterns.
                  , totalPatternTime :: !NominalDiffTime
                    -- ^ The total time spent in pattern execution.
                  , totalSleepTime   :: !NominalDiffTime
                    -- ^ The part of the total pattern time spent
                    -- sleeping.
                  , totalLatencyTime :: !NominalDiffTime
                    -- ^ The part of the total pattern time spent in
                    -- network latency.
                  }
    deriving (Show)

mkGlobalCounter :: GlobalCounter
mkGlobalCounter = GlobalCounter { totalByteCount   = empty
                                , totalActivations = 0
                                , totalPatternTime = toEnum 0
                                , totalSleepTime   = toEnum 0
                                , totalLatencyTime = toEnum 0
                                }
