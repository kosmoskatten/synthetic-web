module TestSuite.Task
       ( vectorLengthIsSumOfWeights
       , namesShallBeEqual
       , patternCountersAsManyAsPatterns
       ) where

import Control.Concurrent.STM (readTVarIO)
import Data.List (foldl')
import qualified Data.Vector as Vector
import Generators.Plan ()
import Test.QuickCheck
import Test.QuickCheck.Monadic
import SyntheticWeb.Counter ( CounterPair (..)
                            , CounterSet (..)
                            , PatternCounter (..) )
import SyntheticWeb.Plan ( Plan (..)
                         , Pattern (..)
                         , Weight (..) )
import SyntheticWeb.Task ( Task (..)
                         , mkTaskSet )

-- | Check that the length of the task vector is equal to the sum of
-- weights.
vectorLengthIsSumOfWeights :: Plan -> Property
vectorLengthIsSumOfWeights plan =
  monadicIO $ do
    (_, v) <- run (mkTaskSet plan)
    assert $ weightSum plan == Vector.length v

-- | Check for each task in the vector that the pattern's name and the
-- pattern counter's name are equal.
namesShallBeEqual :: Plan -> Property
namesShallBeEqual plan =
  monadicIO $ do
    (_, v) <- run (mkTaskSet plan)
    names <- run $ Vector.forM v $
      \(Task task) -> do
        let pName                     = name (fst task)
            CounterPair (pCounter, _) = snd task
        cName <- patternName <$> readTVarIO pCounter
        return (pName, cName)
    Vector.forM names $ assert . uncurry (==)

-- | The number of pattern counters in the counter set must be equal
-- to the number of patterns.
patternCountersAsManyAsPatterns :: Plan -> Property
patternCountersAsManyAsPatterns plan@(Plan patterns) =
  monadicIO $ do
    (cs, _) <- run (mkTaskSet plan)
    let CounterSet (_, patternCounters) = cs
    assert $ length patterns == length patternCounters

weightSum :: Plan -> Int
weightSum (Plan plan) = go plan
    where go = foldl' (\acc (Weight w, _) -> acc + w) 0
