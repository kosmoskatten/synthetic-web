-- | The Plan provides a notation to describe the simulated network
-- traffic in terms of Patterns and Activities.
module SyntheticWeb.Plan       
       ( Bytes
       , Weight (..)
       , Plan
       , Pattern (..)
       , Activity (..)
       , Duration (..)
       , Rate (..)
       , Size (..)
       , Flag (..)
       , parsePlan
       , writePlan
       , expand
       ) where

import Data.Vector (Vector, fromList)
import SyntheticWeb.Plan.Parser
import SyntheticWeb.Plan.Types
import SyntheticWeb.Plan.Writer

-- | Expand the plan such that each pattern is multiplied with its
-- weight.
expand :: Plan -> Vector Pattern
expand = fromList . concatMap (\(Weight w, s) -> replicate w s)
