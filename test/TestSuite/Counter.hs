module TestSuite.Counter
       ( incDownloadWithAmount
       , incUploadWithAmount
       , convertBytesToBits
       ) where

import SyntheticWeb.Counter
import Test.QuickCheck
import GHC.Int (Int64)

instance Arbitrary ByteCounter where
  arbitrary = ByteCounter <$> choose (0, halfMax) <*> choose (0, halfMax)

data TestSpec = TestSpec ByteCounter Int64
  deriving Show

instance Arbitrary TestSpec where
  arbitrary = TestSpec <$> arbitrary <*> choose (0, halfMax - 1)

halfMax :: Int64
halfMax = maxBound `div` 2

incDownloadWithAmount :: TestSpec -> Bool
incDownloadWithAmount (TestSpec orig amount) =
  let new = incDownload amount orig
  in download new == download orig + amount
     && upload new == upload orig

incUploadWithAmount :: TestSpec -> Bool
incUploadWithAmount (TestSpec orig amount) =
  let new = incUpload amount orig
  in upload new == upload orig + amount
     && download new == download orig

convertBytesToBits :: Int64 -> Bool
convertBytesToBits n = bytesToBits n == n * 8
