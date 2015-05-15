module TestSuite.Host
       ( encodeDecodeIsEqual
       ) where

import Data.Word (Word16)
import Test.QuickCheck
import SyntheticWeb.Host (Host (..))
import qualified Data.ByteString.Char8 as BS

instance Arbitrary Host where
  arbitrary = Host <$> hostname' <*> port'

hostname' :: Gen BS.ByteString
hostname' = BS.pack <$> go
    where
      go :: Gen String
      go =          
          let initCharSet = ['a'..'z'] ++ ['A'..'Z']
              contCharSet = initCharSet ++ ['0'..'9'] ++ ['-', '.']
          in (:) <$> elements initCharSet <*> listOf (elements contCharSet)

port' :: Gen Word16
port' = choose (1, maxBound)

encodeDecodeIsEqual :: Host -> Bool
encodeDecodeIsEqual host = host == encodeDecode host
  where
    encodeDecode = read . show
