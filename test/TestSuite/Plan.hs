{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module TestSuite.Plan
       ( encodeDecodePlanIsEqual
       , encodeDecodePlanIsEqualWhenComments
       , encodeDecodeHeaderIsEqual
       ) where

import Control.Monad (replicateM)
import Generators.Plan ()
import Test.QuickCheck
import Text.Parsec (parse)
import SyntheticWeb.Plan
import SyntheticWeb.Plan.Header (HeaderM (..))

data CommentedPlan = CommentedPlan Plan String
  deriving (Eq, Show)

instance Arbitrary CommentedPlan where
  arbitrary = do
    plan <- arbitrary
    let encodedPlan = lines $ writePlan plan
    encodedPlan' <- unlines . concat <$> mapM commentLine encodedPlan
    return $ CommentedPlan plan (encodedPlan')

instance Arbitrary HeaderM where
  arbitrary = elements [minBound..maxBound]

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
encodeDecodeHeaderIsEqual :: HeaderM -> Bool
encodeDecodeHeaderIsEqual header = header == (read . show) header
