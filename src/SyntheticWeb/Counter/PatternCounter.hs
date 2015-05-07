module SyntheticWeb.Counter.PatternCounter
    ( PatternCounter (..)
    , mkPatternCounter
    ) where

import Data.Time (NominalDiffTime)
import SyntheticWeb.Counter.ByteCounter (ByteCounter, empty)
import GHC.Int (Int64)

data PatternCounter =
    PatternCounter { patternName :: !String
                     -- ^ The name of the pattern.
                   , byteCount   :: !ByteCounter
                     -- ^ The number of download and upload bytes for
                     -- (all intances of) the pattern.
                   , activations :: {-# UNPACK #-} !Int64
                     -- ^ The number of activiations for (all
                     -- instances of) the pattern.
                   , patternTime :: !NominalDiffTime
                     -- ^ The time spent executing (all instances of)
                     -- the pattern.
                   , sleepTime   :: !NominalDiffTime
                     -- ^ The part of the pattern time spent sleeping
                     -- in (all instances of) the pattern.
                   , latencyTime :: !NominalDiffTime
                     -- ^ The part of the pattern time spent in
                     -- network latency for (all instances of) the
                     -- pattern.
                   }
    deriving (Show)

mkPatternCounter :: String -> PatternCounter
mkPatternCounter patternName' = 
    PatternCounter { patternName = patternName'
                   , byteCount   = empty
                   , activations = 0
                   , patternTime = toEnum 0
                   , sleepTime   = toEnum 0
                   , latencyTime = toEnum 0
                   }
