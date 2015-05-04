{-# LANGUAGE RecordWildCards #-}
module SyntheticWeb.Counter.Throughput
       ( Throughput (..)
       , toThroughput
       ) where

import SyntheticWeb.Counter.ByteCounter (ByteCounter (..))
import GHC.Int (Int64)

-- | Throughput rates.
data Throughput =
  Bps    !Double
  | Kbps !Double
  | Mbps !Double
  | Gbps !Double
  deriving (Eq, Show)

-- | Convert a byte counter, and a time duration, to a pair of
-- throughputs. The first in the pair is download throughput and the
-- second is upload throughput.
toThroughput :: ByteCounter -> Double -> (Throughput, Throughput)
toThroughput ByteCounter {..} t =
  let dlBits  = bytesToBits download
      ulBits  = bytesToBits upload
      dlBitsT = fromIntegral dlBits / t
      ulBitsT = fromIntegral ulBits / t
  in (toThroughput' dlBitsT, toThroughput' ulBitsT)
    where
      toThroughput' :: Double -> Throughput
      toThroughput' bits
          | bits < 500       = Bps bits
          | bits < 500000    = Kbps (bits * 0.001)
          | bits < 500000000 = Mbps (bits * 0.000001)
          | otherwise        = Gbps (bits * 0.000000001)

bytesToBits :: Int64 -> Int64
bytesToBits = (*) 8
