module Main (main) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified TestSuite.Host as Host
import qualified TestSuite.Plan as Plan
import qualified TestSuite.Server as Server

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
  , testGroup "Server feature tests"
    [ testCase "Get text/plain size 667" Server.getTextPlainSize667
    , testCase "Get text/html size 5671" Server.getTextHtmlSize5671
    , testCase "Put text/plain" Server.putTextPlain
    ]
  ]
