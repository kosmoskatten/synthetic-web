{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module TestSuite.Plan
       ( vectorSizeIsSumOfWeights
       , encodeDecodeIsEqual
       ) where

import Control.Applicative ((<$>), (<*>))
import Data.Attoparsec.ByteString.Char8 (IResult (..), parse)
import qualified Data.ByteString.Char8 as BS
import Data.List (foldl')
import qualified Data.Vector as Vector
import Test.QuickCheck
import SyntheticWeb.Plan

instance Arbitrary Pattern where
  arbitrary = Pattern <$> patternName <*> (resize 20 $ listOf1 spec)
    where
      spec :: Gen Activity
      spec = arbitrary

instance Arbitrary Activity where
  arbitrary = oneof [ SLEEP  <$> arbitrary
                    , GET    <$> arbitrary <*> arbitrary <*> arbitrary
                    , PUT    <$> arbitrary <*> arbitrary <*> arbitrary
                    , Chatty <$> arbitrary <*> arbitrary <*>
                                 arbitrary <*> arbitrary ]

instance Arbitrary Duration where
  arbitrary = oneof [ Us <$> choose (1, 1000)
                    , Ms <$> choose (1, 1000)
                    , S  <$> choose (1, 1000) ]

instance Arbitrary Rate where
  arbitrary = oneof [ return Unlimited
                    , LimitedTo <$> arbitrary ]

instance Arbitrary Size where
  arbitrary = oneof [ Exactly <$> bytes
                    , Uniform <$> range
                    , Gauss   <$> range ]
    where
      range :: Gen (Bytes, Bytes)
      range = (,) <$> bytes <*> bytes

instance Arbitrary Weight where
  arbitrary = Weight <$> choose (1, 10)

instance Arbitrary Flag where
  arbitrary = elements [minBound..maxBound]

bytes :: Gen Bytes
bytes = choose (500, 50000000)

patternName :: Gen String
patternName =
  resize 20 $ listOf1 $
    elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_', '-'])

-- | Check that the plan is expanded such that the expansion's length
-- is equal to the sum of the weights.
vectorSizeIsSumOfWeights :: Plan -> Bool
vectorSizeIsSumOfWeights plan =
  let v = expand plan
  in Vector.length v == weightSum plan

encodeDecodeIsEqual :: Plan -> Bool
encodeDecodeIsEqual plan =
  case extractResult (parse parsePlan $ writePlan plan) of
    Just result -> result == plan
    Nothing     -> False

extractResult :: IResult BS.ByteString Plan -> Maybe Plan
extractResult (Partial cont) = extractResult $ cont ""
extractResult (Done _ r)     = Just r
extractResult _              = Nothing

weightSum :: Plan -> Int
weightSum = foldl' (\acc (Weight w, _) -> acc + w) 0
