module Generators.Plan where

import Test.QuickCheck
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
  arbitrary = oneof [ Usec <$> choose (1, 1000)
                    , Msec <$> choose (1, 1000)
                    , Sec  <$> choose (1, 1000) ]

instance Arbitrary Download where
  arbitrary = Download <$> arbitrary

instance Arbitrary Upload where
  arbitrary = Upload <$> arbitrary

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
patternName = (:) <$> elements initSet <*> contSet 3 25
  where
    initSet :: String
    initSet = ['A'..'Z'] ++ ['a'..'z']

    contSet :: Int -> Int -> Gen String
    contSet minLength maxLength = do
      len <- choose (minLength, maxLength)
      take len <$> infiniteListOf (elements $ initSet ++ ['0'..'9'] ++ "-_.")
