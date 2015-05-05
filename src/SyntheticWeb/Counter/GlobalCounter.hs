module SyntheticWeb.Counter.GlobalCounter
    ( GlobalCounter (..)
    , mkGlobalCounter
    ) where

import SyntheticWeb.Counter.ByteCounter (ByteCounter, empty)

data GlobalCounter =
    GlobalCounter { totalBytes :: !ByteCounter }
    deriving (Show)

mkGlobalCounter :: GlobalCounter
mkGlobalCounter = GlobalCounter { totalBytes = empty }
