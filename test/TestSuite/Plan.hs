{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module TestSuite.Plan
       ( encodeDecodeIsEqual
       , encodeDecodeIsEqualWhenComments
       ) where

import Control.Monad (replicateM)
import Generators.Plan ()
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
    return $ CommentedPlan plan (encodedPlan')

commentLine :: String -> Gen [String]
commentLine str = do
  num      <- choose (0, 3)
  comments <- replicateM num comment
  return $ comments ++ [str]
  where
    comment = ('#':) <$> listOf (elements [' '..'z'])

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
