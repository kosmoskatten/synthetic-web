-- | The Plan provides a notation to describe the simulated network
-- traffic in terms of Sessions and Activities.

{-# LANGUAGE DeriveGeneric #-}
module QuickNet.Plan
       ( Plan
       , SessionSpec (..)
       , Session (..)         
       , ActivitySpec (..)
       , Activity (..)
       , Rate (..)
       , Size (..)
       ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | Weights are used to define probabilities for sessions and
-- activities.
type Weight = Int

-- | Tell the number of bytes.
type Bytes = Int

-- | The plan is a list of weighted sessions. All weights are summed,
-- and the probability of a session is proportional to the total
-- weight sum.
type Plan = [ SessionSpec ]

-- | Specification of a weighted session.
data SessionSpec =
  SessionSpec { sessionWeight :: !Weight
              , session       :: !Session }
  deriving (Generic, Read, Show)

data Session =
  Session { name       :: !String
          , activities :: ![ActivitySpec]
          }
  deriving (Generic, Read, Show)

data ActivitySpec =
  ActivitySpec { activityWeight :: !Weight
               , rate           :: !Rate
               , activity       :: !Activity }
  deriving (Generic, Read, Show)

-- | Definition of activities.
data Activity =
  Sleep Int
    -- ^ Sleep the specified number of milliseconds.
  | GET Size
    -- ^ Fetch a resource with the specified size.
  | PUT Size
    -- ^ Upload a resource with the specified size.
  | Chatty Size Size
    -- ^ Perform a chatty conversation. The first size specifies the
    -- chunk size, and the second size specified the complete data
    -- size for the conversation.
  deriving (Generic, Read, Show)

data Rate =
  Unlimited
    -- ^ Unlimited bitrate.
  | LimitedTo Size
    -- ^ Limited to bytes/s.
  deriving (Generic, Read, Show)

-- | Specification of a requested size measured in bytes. The size
-- could be expressed exactly or as a range specified for a random
-- distribution.
data Size =
  Exactly Bytes
    -- ^ Exactly the number of bytes.
  | Uniform (Bytes, Bytes)
    -- ^ A number of bytes uniformly distributed over the range.
  | Gauss (Bytes, Bytes)
    -- ^ A numer of bytes normal distributed over the range.
  deriving (Generic, Read, Show)

instance FromJSON ActivitySpec
instance FromJSON Activity
instance FromJSON Rate
instance FromJSON SessionSpec
instance FromJSON Session
instance FromJSON Size

instance ToJSON ActivitySpec
instance ToJSON Activity
instance ToJSON Rate
instance ToJSON SessionSpec
instance ToJSON Session
instance ToJSON Size
