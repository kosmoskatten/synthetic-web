{-# LANGUAGE RecordWildCards #-}
module SyntheticWeb.Counter.Throughput
       ( Throughput (..)
       , toThroughput
       ) where

import SyntheticWeb.Counter.ByteCounter (ByteCounter (..))
import GHC.Int (Int64)

data Throughput =
  Bps    !Double
  | Kbps !Double
  | Mbps !Double
  | Gbps !Double
  deriving (Eq, Show)

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
          | bits < 500000    = Kbps bits
          | bits < 500000000 = Mbps bits
          | otherwise        = Gbps bits

bytesToBits :: Int64 -> Int64
bytesToBits = (*) 8
