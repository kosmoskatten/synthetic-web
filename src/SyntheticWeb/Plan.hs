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
       , Download (..)
       , Upload (..)
       , Size (..)
       , Header (..)
       , parsePlan
       , writePlan
       ) where

import SyntheticWeb.Plan.Parser
import SyntheticWeb.Plan.Types
import SyntheticWeb.Plan.Writer
