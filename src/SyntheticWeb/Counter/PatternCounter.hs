module SyntheticWeb.Counter.PatternCounter
    ( PatternCounter (..)
    , mkPatternCounter
    ) where

import SyntheticWeb.Counter.ByteCounter (ByteCounter, empty)

data PatternCounter =
    PatternCounter { patternName :: !String
                   , bytes       :: !ByteCounter }
    deriving (Show)

mkPatternCounter :: String -> PatternCounter
mkPatternCounter patternName' = 
    PatternCounter { patternName = patternName'
                   , bytes       = empty }
