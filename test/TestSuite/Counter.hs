{-# LANGUAGE RecordWildCards #-}
module TestSuite.Counter
       ( byteCounterArithmetic
       , throughputThresholds
       , activationCounterPropagation
       , byteCounterPropagation
       , patternTimeCounterPropagation
       , sleepTimeCounterPropagation
       , latencyTimeCounterPropagation
       , convert1ByteToThroughput
       ) where

import Control.Concurrent.STM (STM)
import Control.Monad (forM_)
import Data.Time (NominalDiffTime)
import qualified Data.Vector as Vector
import Generators.Plan ()
import SyntheticWeb.Counter
import SyntheticWeb.Counter.ByteCounter
import SyntheticWeb.Counter.Throughput
import SyntheticWeb.Plan
import SyntheticWeb.Task
import Test.HUnit hiding (assert)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import GHC.Int (Int64)

instance Arbitrary ByteCounter where
  arbitrary = ByteCounter <$> choose (0, 2000000)
                          <*> choose (0, 2000000)

byteCounterArithmetic :: ByteCounter -> ByteCounter -> Bool
byteCounterArithmetic b1 b2 =
  let b3 = b1 `addByteCount` b2
  in download b3 == download b2 + download b1
     && upload b3 == upload b2 + upload b1

throughputThresholds :: ByteCounter -> Bool
throughputThresholds byteCounter@ByteCounter {..} =
    let (dl, ul) = toThroughput byteCounter 1
    in dl `matches` (download * 8) && ul `matches` (upload * 8)
    where
      matches :: Throughput -> Int64 -> Bool
      matches (Bps bits)  bits' =
        -- The throughput shall not be scaled.
        bits' < 500 && bits == fromIntegral bits'
      matches (Kbps bits) bits' =
        -- The throughput shall be scaled down by 1000
        bits' < 500000 && bits == fromIntegral bits' * 0.001
      matches (Mbps bits) bits' =
        -- The throughput shall be scaled down by 1000000
        bits' < 500000000 && bits == fromIntegral bits' * 0.000001
      matches (Gbps bits) bits' =
        -- The throughput shall be scaled down by 1000000000
        bits' >= 500000000 && bits == fromIntegral bits' * 0.000000001

activationCounterPropagation :: Plan -> Property
activationCounterPropagation plan@(Plan plan') =
  monadicIO $ do
    (counters, tasks) <- run (mkTaskSet plan)
    run $ Vector.mapM_ (atomically . activatePattern . counterFrom) tasks

    FrozenSet (_, g, ps) <- run (freeze counters)
    -- Check that the total activations is equal to the number of
    -- tasks.
    assert $ (fromIntegral $ Vector.length tasks) == totalActivations g

    -- Check that the activations for each pattern is equal to the
    -- pattern's weight.
    forM_ plan' $ \(Weight w, p) -> do
      let [patternCounter] = filter (\c -> patternName c == name p) ps
      assert $ (fromIntegral w) == activations patternCounter      

byteCounterPropagation :: Plan -> Property
byteCounterPropagation plan@(Plan plan') =
  monadicIO $ do
    (counters, tasks) <- run (mkTaskSet plan)
    let bytes = ByteCounter { download = 1, upload = 2 }
    run $ Vector.mapM_ (atomically . updateByteCount bytes . counterFrom) tasks

    FrozenSet (_, g, ps) <- run (freeze counters)
    -- Check that the total download bytes is equal to the number of
    -- tasks, and that the upload bytes are twice as big.
    let numTasks = fromIntegral $ Vector.length tasks
    assert $ numTasks == download (totalByteCount g)
    assert $ 2 * numTasks == upload (totalByteCount g)

    -- Check that the counters for each pattern has the same
    -- proportions.
    forM_ plan' $ \(Weight w, p) -> do
      let [patternCounter] = filter (\c -> patternName c == name p) ps
          w'               = fromIntegral w
      assert $ w' == download (byteCount patternCounter)
      assert $ 2 * w' == upload (byteCount patternCounter)

patternTimeCounterPropagation :: Plan -> Property
patternTimeCounterPropagation =
  timeCounterPropagation updatePatternTime totalPatternTime patternTime

sleepTimeCounterPropagation :: Plan -> Property
sleepTimeCounterPropagation =
  timeCounterPropagation updateSleepTime totalSleepTime sleepTime

latencyTimeCounterPropagation :: Plan -> Property
latencyTimeCounterPropagation =
  timeCounterPropagation updateLatencyTime totalLatencyTime latencyTime

timeCounterPropagation :: (NominalDiffTime -> CounterPair -> STM ())
                       -> (GlobalCounter -> NominalDiffTime)
                       -> (PatternCounter -> NominalDiffTime)
                       -> Plan
                       -> Property
timeCounterPropagation setter ggetter pgetter plan@(Plan plan') =
  monadicIO $ do
    (counters, tasks) <- run (mkTaskSet plan)
    let delta = toEnum 1
    run $ Vector.mapM_ (atomically . setter delta . counterFrom) tasks

    FrozenSet (_, g, ps) <- run (freeze counters)
    -- Check that the total time is equal to the "time" proportial to
    -- the number of tasks.
    let totTime = toEnum $ Vector.length tasks
    assert $ totTime == ggetter g

    -- Check that the time for each pattern are the same as the
    -- weight.
    forM_ plan' $ \(Weight w, p) -> do
      let [patternCounter] = filter (\c -> patternName c == name p) ps
          delta'           = toEnum w
      assert $ delta' == pgetter patternCounter

convert1ByteToThroughput :: Assertion
convert1ByteToThroughput = do
    let counter1 = ByteCounter 1 0
        counter2 = ByteCounter 0 1
        counter3 = ByteCounter 1 1
    -- For one second duration.
    assertEqual "Shall be equal" (Bps 8, Bps 0) $ toThroughput counter1 1.0
    assertEqual "Shall be equal" (Bps 0, Bps 8) $ toThroughput counter2 1.0
    assertEqual "Shall be equal" (Bps 8, Bps 8) $ toThroughput counter3 1.0
    -- For two seconds duration.
    assertEqual "Shall be equal" (Bps 4, Bps 0) $ toThroughput counter1 2.0
    assertEqual "Shall be equal" (Bps 0, Bps 4) $ toThroughput counter2 2.0
    assertEqual "Shall be equal" (Bps 4, Bps 4) $ toThroughput counter3 2.0
