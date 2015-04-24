{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module TestSuite.Plan
       ( vectorSizeIsSumOfWeights
       , encodeDecodeIsEqual
       ) where

import Control.Applicative ((<$>), (<*>))
--import Data.Attoparsec.ByteString.Char8 (IResult (..), parse)
import qualified Data.ByteString.Char8 as BS
import Data.List (foldl')
import qualified Data.Vector as Vector
import Test.QuickCheck
import Text.Parsec (parse)
import SyntheticWeb.Plan

instance Arbitrary Plan where
  arbitrary = Plan <$> (resize 20 $ listOf weightPatternPair)

instance Arbitrary Pattern where
  arbitrary = Pattern <$> patternName <*> (resize 20 $ listOf1 spec)
    where
      spec :: Gen Activity
      spec = arbitrary

instance Arbitrary Activity where
  arbitrary = oneof [ SLEEP  <$> arbitrary
                    , GET    <$> arbitrary <*> arbitrary <*> arbitrary
                    , PUT    <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Arbitrary Duration where
  arbitrary = oneof [ Us <$> choose (1, 1000)
                    , Ms <$> choose (1, 1000)
                    , S  <$> choose (1, 1000) ]

instance Arbitrary Payload where
  arbitrary = Payload <$> arbitrary

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

instance Arbitrary Header where
  arbitrary = elements [minBound..maxBound]

weightPatternPair :: Gen (Weight, Pattern)
weightPatternPair = (,) <$> arbitrary <*> arbitrary

bytes :: Gen Bytes
bytes = choose (500, 50000000)

patternName :: Gen String
patternName =
  resize 20 $ listOf1 $
    elements (['A'..'Z'])

-- | Check that the plan is expanded such that the expansion's length
-- is equal to the sum of the weights.
vectorSizeIsSumOfWeights :: Plan -> Bool
vectorSizeIsSumOfWeights plan =
  let v = expand plan
  in Vector.length v == weightSum plan

encodeDecodeIsEqual :: Plan -> Bool
encodeDecodeIsEqual plan =
  case parse parsePlan2 "" $ writePlan plan of
    Right plan' -> plan' == plan
    Left _      -> False

weightSum :: Plan -> Int
weightSum (Plan plan) = go plan
    where go = foldl' (\acc (Weight w, _) -> acc + w) 0
