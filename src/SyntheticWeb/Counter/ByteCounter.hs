{-# LANGUAGE RecordWildCards #-}
module SyntheticWeb.Counter.ByteCounter
       ( ByteCounter (..)
       , empty
       , incDownload
       , incUpload
       ) where

import GHC.Int (Int64)

data ByteCounter =
  ByteCounter { download :: !Int64
              , upload   :: !Int64 }
  deriving (Show)

empty :: ByteCounter
empty = ByteCounter 0 0

incDownload :: Int64 -> ByteCounter -> ByteCounter
incDownload amount bc@ByteCounter {..} = bc { download = download + amount }

incUpload :: Int64 -> ByteCounter -> ByteCounter
incUpload amount bc@ByteCounter {..} = bc { upload = upload + amount }
