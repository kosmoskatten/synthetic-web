-- | The Plan provides a notation to describe the simulated network
-- traffic in terms of Patterns and Activities.
module SyntheticWeb.Plan       
       ( Plan (..)
       , Bytes
       , Weight (..)
       , Pattern (..)
       , Activity (..)
       , Duration (..)
       , Rate (..)
       , Payload (..)
       , Size (..)
       , Header (..)
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
expand (Plan plan) = go plan
    where go = fromList . concatMap (\(Weight w, s) -> replicate w s)
