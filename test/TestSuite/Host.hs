module TestSuite.Host
       ( encodeDecodeIsEqual
       ) where

import Test.QuickCheck
import SyntheticWeb.Host (Host (..))

instance Arbitrary Host where
  arbitrary = Host <$> hostname' <*> port'

hostname' :: Gen String
hostname' = let initCharSet = ['a'..'z'] ++ ['A'..'Z']
                contCharSet = initCharSet ++ ['0'..'9'] ++ ['-', '.']
            in (:) <$> elements initCharSet <*> listOf (elements contCharSet)

port' :: Gen Int
port' = choose (1, maxBound)

encodeDecodeIsEqual :: Host -> Bool
encodeDecodeIsEqual host = host == encodeDecode host
  where
    encodeDecode = read . show
