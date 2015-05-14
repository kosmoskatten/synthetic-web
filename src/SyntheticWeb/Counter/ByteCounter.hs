module SyntheticWeb.Counter.ByteCounter
       ( ByteCounter (..)
       , empty
       , addByteCount
       ) where

import GHC.Int (Int64)

data ByteCounter =
  ByteCounter { download :: {-# UNPACK #-} !Int64
              , upload   :: {-# UNPACK #-} !Int64 }
  deriving (Show)

empty :: ByteCounter
empty = ByteCounter 0 0

addByteCount :: ByteCounter -> ByteCounter -> ByteCounter
addByteCount b1 b2 = ByteCounter { download = download b1 + download b2
                                 , upload   = upload b1   + upload b2
                                 }
