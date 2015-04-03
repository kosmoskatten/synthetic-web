module Main (main) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified TestSuite.Host as Host
import qualified TestSuite.Plan as Plan

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
  [ testGroup "Plan property tests"
    [ testProperty "VectorSizeIsSumOfWeights" Plan.vectorSizeIsSumOfWeights
    , testProperty "EncodeDecodeIsEqual" Plan.encodeDecodeIsEqual
    ]
  , testGroup "Host property tests"
    [ testProperty "DecodeEncodeIsEqual" Host.encodeDecodeIsEqual
    ]
  ]
