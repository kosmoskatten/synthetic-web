{-# LANGUAGE RecordWildCards #-}
module TestSuite.Counter
       ( incDownloadWithAmount
       , incUploadWithAmount
       , throughputThresholds
       , convert1ByteToThroughput
       ) where

import SyntheticWeb.Counter
import Test.HUnit
import Test.QuickCheck
import GHC.Int (Int64)

instance Arbitrary ByteCounter where
  arbitrary = ByteCounter <$> choose (0, 2000000)
                          <*> choose (0, 2000000)

data IncSpec = IncSpec ByteCounter Int64
  deriving Show

instance Arbitrary IncSpec where
  arbitrary = IncSpec <$> arbitrary <*> choose (0, 2000000)

incDownloadWithAmount :: IncSpec -> Bool
incDownloadWithAmount (IncSpec orig amount) =
  let new = incDownload amount orig
  in download new == download orig + amount
     && upload new == upload orig

incUploadWithAmount :: IncSpec -> Bool
incUploadWithAmount (IncSpec orig amount) =
  let new = incUpload amount orig
  in upload new == upload orig + amount
     && download new == download orig

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
