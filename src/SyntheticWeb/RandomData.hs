module SyntheticWeb.RandomData
    ( randomData
    ) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Random (mkStdGen, randomRs)

randomData :: LBS.ByteString
randomData = go (mkStdGen 42)
  where go = LBS.pack . randomRs ('0', 'z')
