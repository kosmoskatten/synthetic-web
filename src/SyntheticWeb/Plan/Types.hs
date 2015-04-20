module SyntheticWeb.Plan.Types
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
       ) where

-- | Tell the number of bytes.
type Bytes = Int

-- | Weights are used to define probabilities for pattern and
-- activities.
newtype Weight = Weight Int
  deriving (Eq, Show)

-- | The plan is a list of weighted pattern. All weights are summed,
-- and the probability of a pattern is proportional to the total
-- weight sum.
newtype Plan = Plan [ (Weight, Pattern) ]
  deriving (Eq, Show)

data Pattern =
  Pattern { name       :: !String
          , activities :: ![ Activity ]
          }
  deriving (Eq, Show)

-- | Definition of activities.
data Activity =
  SLEEP !Duration
    -- ^ Sleep during the specified duration.
  | GET ![Header] !Payload !Rate
    -- ^ Fetch a resource with the specified size.
  | PUT ![Header] !Payload !Rate
    -- ^ Upload a resource with the specified size.
  deriving (Eq, Show)

-- | Specification of a duration.
data Duration =
  Us !Int
    -- ^ Duration in microseconds.
  | Ms !Int
    -- ^ Duration in milliseconds.
  | S !Int
    -- ^ Duration in seconds.
  deriving (Eq, Show)

-- | Specification of payload.
newtype Payload = Payload Size
  deriving (Eq, Show)

-- | Specification of rate limitation. 
data Rate =
  Unlimited
    -- ^ Unlimited bitrate.
  | LimitedTo !Size
    -- ^ Limited to bytes/s.
  deriving (Eq, Show)

-- | Specification of a requested size measured in bytes. The size
-- could be expressed exactly or as a range specified for a random
-- distribution.
data Size =
  Exactly !Bytes
    -- ^ Exactly the number of bytes.
  | Uniform !(Bytes, Bytes)
    -- ^ A number of bytes uniformly distributed over the range.
  | Gauss !(Bytes, Bytes)
    -- ^ A numer of bytes normal distributed over the range.
  deriving (Eq, Show)

-- | Header flags that specifies the behavior of the communication
-- between client and server.
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
  deriving (Bounded, Enum, Eq, Read, Show)
