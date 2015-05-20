{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module SyntheticWeb.Plan.Types
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
       ) where

import Control.DeepSeq
import GHC.Generics
import SyntheticWeb.Statistical (Statistical (..))

-- | Tell the number of bytes.
type Bytes = Int

-- | Weights are used to define probabilities for pattern and
-- activities.
newtype Weight = Weight Int
  deriving (Eq, Generic, NFData, Show)

-- | The plan is a list of weighted pattern. All weights are summed,
-- and the probability of a pattern is proportional to the total
-- weight sum.
newtype Plan = Plan [ (Weight, Pattern) ]
  deriving (Eq, Generic, NFData, Show)

data Pattern =
  Pattern { name       :: !String
          , activities :: ![ Activity ]
          }
  deriving (Eq, Generic, NFData, Show)

-- | Definition of activities.
data Activity =
  SLEEP !Duration
    -- ^ Sleep during the specified duration.
  | GET ![Header] !Download !Rate
    -- ^ Fetch a resource with the specified payload size and the
    -- specified rate for download payload.
  | PUT ![Header] !Upload
    -- ^ Upload a resource with the specified size for the upload
    -- payload. PUT is not expected to have any other download than
    -- the HTTP reply (i.e. no payload data).
  | POST ![Header] !Upload !Download !Rate
    -- ^ Create a resource with the specified sizes for upload and
    -- download payloads and the specified rate for the download
    -- payload.
  deriving (Eq, Generic, NFData, Show)

-- | Specification of a duration.
data Duration =
  Usec !(Statistical Int)
    -- ^ Duration in microseconds.
  | Msec !(Statistical Int)
    -- ^ Duration in milliseconds.
  | Sec !(Statistical Int)
    -- ^ Duration in seconds.
  deriving (Eq, Generic, NFData, Show)

-- | Specification of download payload size.
newtype Download = Download Size
  deriving (Eq, Generic, NFData, Show)

-- | Specification of upload payload size.
newtype Upload = Upload Size
  deriving (Eq, Generic, NFData, Show)

-- | Specification of rate limitation. 
data Rate =
  Unlimited
    -- ^ Unlimited bitrate.
  | LimitedTo !Size
    -- ^ Limited to bytes/s.
  deriving (Eq, Generic, NFData, Show)

-- | Specification of a requested size measured in bytes
data Size = Size !(Statistical Bytes)
  deriving (Eq, Generic, NFData, Show)

-- | Header flags that specifies the behavior of the communication
-- between client and server.t
data Header =
  AcceptAny
    -- ^ The client accepts contents of any type in the response.
  | AcceptTextHtml
    -- ^ The client accepts text/html as content in the response.
  | AcceptTextPlain
    -- ^ The client accepts text/plain as content in the response.
  | AcceptApplicationJSON
    -- ^ The client accepts application/json as content in the response.
  | ContentTextHtml
    -- ^ The content in the upstream request is text/html.
  | ContentTextPlain
    -- ^ The content in the upstream request is text/plain.
  | ContentApplicationJSON
    -- ^ The content in the upstream request is application/json.
  deriving (Bounded, Enum, Eq, Generic, NFData, Show)

