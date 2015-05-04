module SyntheticWeb.Counter.Throughput
       ( bytesToBits
       ) where

import SyntheticWeb.Counter.ByteCounter (ByteCounter (..))
import GHC.Int (Int64)

data Throughput =
  Bps    !Double
  | Kbps !Double
  | Mbps !Double
  | Gbps !Double
  deriving (Show)

bytesToBits :: Int64 -> Int64
bytesToBits = (*) 8
