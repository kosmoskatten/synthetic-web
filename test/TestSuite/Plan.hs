{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module TestSuite.Plan
       ( encodeDecodePlanIsEqual
       , encodeDecodePlanIsEqualWhenComments
       , encodeDecodeHeaderIsEqual
       , shallTranslateToHeaderTuples
       ) where

import Control.Monad (replicateM)
import Generators.Plan ()
import Test.HUnit
import Test.QuickCheck
import Text.Parsec (parse)
import SyntheticWeb.Plan

data CommentedPlan = CommentedPlan Plan String
  deriving (Eq, Show)

instance Arbitrary CommentedPlan where
  arbitrary = do
    plan <- arbitrary
    let encodedPlan = lines $ writePlan plan
    encodedPlan' <- unlines . concat <$> mapM commentLine encodedPlan
    return $ CommentedPlan plan encodedPlan'

commentLine :: String -> Gen [String]
commentLine str = do
  num      <- choose (0, 3)
  comments <- replicateM num comment
  return $ comments ++ [str]
  where
    comment = ('#':) <$> listOf (elements [' '..'z'])

-- | From a plan, encode and decode it. The new plan shall be equal to
-- the original plan.
encodeDecodePlanIsEqual :: Plan -> Bool
encodeDecodePlanIsEqual plan =
  case parse parsePlan "" $ writePlan plan of
    Right plan' -> plan' == plan
    Left _      -> False

-- | From an encoded plan decorated with comments, the decoded plan
-- shall be equal to the original plan.
encodeDecodePlanIsEqualWhenComments :: CommentedPlan -> Bool
encodeDecodePlanIsEqualWhenComments (CommentedPlan plan encodedPlan) =
  case parse parsePlan "" encodedPlan of
    Right plan' -> plan' == plan
    Left _      -> False

-- | Test that read and show convert correctly between type and string
-- representions.
encodeDecodeHeaderIsEqual :: Header -> Bool
encodeDecodeHeaderIsEqual header = header == (read . show) header

-- | Check that header tags are translated to tuples.
shallTranslateToHeaderTuples :: Assertion
shallTranslateToHeaderTuples = do
  ("Accept", "text/html")        @=? toTuple AcceptTextHtml
  ("Accept", "text/plain")       @=? toTuple AcceptTextPlain
  ("Accept", "application/json") @=? toTuple AcceptApplicationJson
  ("Accept", "application/xml")  @=? toTuple AcceptApplicationXml
  ("Accept", "image/jpeg")       @=? toTuple AcceptImageJpeg
  ("Accept", "video/mpeg")       @=? toTuple AcceptVideoMpeg
  ("Accept", "audio/mpeg")       @=? toTuple AcceptAudioMpeg

  ("Content-Type", "text/html")        @=? toTuple ContentTextHtml
  ("Content-Type", "text/plain")       @=? toTuple ContentTextPlain
  ("Content-Type", "application/json") @=? toTuple ContentApplicationJson
  ("Content-Type", "application/xml")  @=? toTuple ContentApplicationXml
  ("Content-Type", "image/jpeg")       @=? toTuple ContentImageJpeg
  ("Content-Type", "video/mpeg")       @=? toTuple ContentVideoMpeg
  ("Content-Type", "audio/mpeg")       @=? toTuple ContentAudioMpeg
