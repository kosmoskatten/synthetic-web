module Generators.Plan where

import Test.QuickCheck
import SyntheticWeb.Plan
import System.Random (Random ())
import SyntheticWeb.Statistical

instance Arbitrary Plan where
  arbitrary = Plan <$> resize 20 (listOf weightPatternPair)

instance Arbitrary Pattern where
  arbitrary = Pattern <$> patternName <*> resize 20 (listOf1 spec)
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
  arbitrary = oneof [ Usec <$> arbitrary
                    , Msec <$> arbitrary
                    , Sec  <$> arbitrary ]

instance Arbitrary Download where
  arbitrary = Download <$> arbitrary

instance Arbitrary Upload where
  arbitrary = Upload <$> arbitrary

instance Arbitrary Rate where
  arbitrary = oneof [ return Unlimited
                    , LimitedTo <$> arbitrary ]

instance Arbitrary Size where
  arbitrary = Size <$> arbitrary

instance (Num a, Random a) => Arbitrary (Statistical a) where
  arbitrary = oneof [ Exactly  <$> val
                    , Uniform  <$> pair
                    , Gaussian <$> pair ]
    where
      pair = (,) <$> val <*> val
      val  = choose (1, 5000000)

instance Arbitrary Weight where
  arbitrary = Weight <$> choose (1, 10)

instance Arbitrary Header where
  arbitrary = elements [minBound..maxBound]

weightPatternPair :: Gen (Weight, Pattern)
weightPatternPair = (,) <$> arbitrary <*> arbitrary

patternName :: Gen String
patternName = (:) <$> elements initSet <*> contSet 3 25
  where
    initSet :: String
    initSet = ['A'..'Z'] ++ ['a'..'z']

    contSet :: Int -> Int -> Gen String
    contSet minLength maxLength = do
      len <- choose (minLength, maxLength)
      take len <$> infiniteListOf (elements $ initSet ++ ['0'..'9'] ++ "-_.")
