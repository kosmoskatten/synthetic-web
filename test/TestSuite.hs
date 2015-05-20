module Main (main) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified TestSuite.Client as Client
import qualified TestSuite.Counter as Counter
import qualified TestSuite.Host as Host
import qualified TestSuite.Plan as Plan
import qualified TestSuite.Server as Server
import qualified TestSuite.Task as Task

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
  [ testGroup "Counter property tests"
    [ testProperty "ByteCounterArithmetic" Counter.byteCounterArithmetic
    , testProperty "ThroughputThresholds" Counter.throughputThresholds
    , testProperty "ActivationCounterPropagation"
                   Counter.activationCounterPropagation
    , testProperty "ByteCounterPropagation" Counter.byteCounterPropagation
    , testProperty "PatternTimeCounterPropagation"
                   Counter.patternTimeCounterPropagation
    , testProperty "SleepTimeCounterPropagation"
                   Counter.sleepTimeCounterPropagation
    , testProperty "LatencyTimeCounterPropagatino"
                   Counter.latencyTimeCounterPropagation
    ]
  , testGroup "Counter unit tests"
    [ testCase "Convert1ByteToThroughput" Counter.convert1ByteToThroughput
    ]
  , testGroup "Plan property tests"
    [ testProperty "EncodeDecodePlanIsEqual" Plan.encodeDecodePlanIsEqual
    , testProperty "EncodeDecodePlanIsEqualWhenComments"
                   Plan.encodeDecodePlanIsEqualWhenComments
    , testProperty "EncodeDecodeHeaderIsEqual" Plan.encodeDecodePlanIsEqual
    ]
  , testGroup "Task property tests"
    [ testProperty "VectorLengthIsSumOfWeights" Task.vectorLengthIsSumOfWeights
    , testProperty "NamesShallBeEqual" Task.namesShallBeEqual
    , testProperty "PatternCountersAsManyAsPatterns"
                   Task.patternCountersAsManyAsPatterns
    ]
  , testGroup "Host property tests"
    [ testProperty "DecodeEncodeIsEqual" Host.encodeDecodeIsEqual
    ]
  , testGroup "Client unit tests"
    [ testCase "TimedActionShallTimeCorrectly" 
               Client.timedActionShallTimeCorrectly
    ]
  , testGroup "Server unit tests"
    [ testCase "Get text/plain size 667" Server.getTextPlainSize667
    , testCase "Get text/html size 5671" Server.getTextHtmlSize5671
    , testCase "Put text/plain" Server.putTextPlain
    , testCase "Post text/html size 1234" Server.postTextHtmlSize1234
    ]
  ]
