{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module TestSuite.Plan
       ( vectorSizeIsSumOfWeights
       , encodeDecodeIsEqual
       , encodeDecodeIsEqualWhenComments
       ) where

import Control.Monad (replicateM)
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
                    , PUT    <$> arbitrary <*> arbitrary
                    , POST   <$> arbitrary <*> arbitrary
                             <*> arbitrary <*> arbitrary ]

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
patternName = (:) <$> elements initSet <*> listOf (elements contSet)
  where
    initSet = ['A'..'Z']
    contSet = initSet ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_."

data CommentedPlan = CommentedPlan Plan BS.ByteString
  deriving (Eq, Show)

instance Arbitrary CommentedPlan where
  arbitrary = do
    plan <- arbitrary
    let encodedPlan = BS.lines $ writePlan plan
    encodedPlan' <- BS.unlines . concat <$> mapM commentLine encodedPlan
    return $ CommentedPlan plan (encodedPlan')

commentLine :: BS.ByteString -> Gen [BS.ByteString]
commentLine str = do
  num      <- choose (0, 3)
  comments <- replicateM num comment
  return $ comments ++ [str]
  where
    comment = BS.pack <$> (('#':) <$> listOf (elements [' '..'z']))

-- | Check that the plan is expanded such that the expansion's length
-- is equal to the sum of the weights.
vectorSizeIsSumOfWeights :: Plan -> Bool
vectorSizeIsSumOfWeights plan =
  let v = expand plan
  in Vector.length v == weightSum plan

-- | From a plan, encode and decode it. The new plan shall be equal to
-- the original plan.
encodeDecodeIsEqual :: Plan -> Bool
encodeDecodeIsEqual plan =
  case parse parsePlan "" $ writePlan plan of
    Right plan' -> plan' == plan
    Left _      -> False

-- | From an encoded plan decorated with comments, the decoded plan
-- shall be equal to the original plan.
encodeDecodeIsEqualWhenComments :: CommentedPlan -> Bool
encodeDecodeIsEqualWhenComments (CommentedPlan plan encodedPlan) =
  case parse parsePlan "" encodedPlan of
    Right plan' -> plan' == plan
    Left _      -> False

weightSum :: Plan -> Int
weightSum (Plan plan) = go plan
    where go = foldl' (\acc (Weight w, _) -> acc + w) 0
