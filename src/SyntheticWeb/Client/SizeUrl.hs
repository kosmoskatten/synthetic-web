module SyntheticWeb.Client.SizeUrl
    ( SizeUrl
    , Payload
    , Url
    , fromSize
    , toUrl
    , toPayload
    ) where

import Control.Monad.IO.Class (MonadIO)
import SyntheticWeb.Plan.Types (Bytes, Size (..))
import SyntheticWeb.Statistical (Statistical (Exactly), sample)
import System.Random.MWC (GenIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

type Url = BS.ByteString
type Payload = LBS.ByteString

-- | Representation of a "size url". An url where a number - integer
-- based size - shall be translated to an url or a sized payload.
newtype SizeUrl = SizeUrl Bytes
  deriving (Show)

-- | Create a SizeUrl from a Size.
fromSize :: MonadIO m => Size -> GenIO -> m SizeUrl
fromSize (Size stat) gen = do
  Exactly bytes <- sample stat gen
  return $ SizeUrl bytes

-- | Translate the SizeUrl to an url.
toUrl :: SizeUrl -> Url
toUrl (SizeUrl bytes) = '/' `BS.cons` BS.pack (show bytes)

-- | Translate the SizeUrl to an equally sized payload.
toPayload :: SizeUrl -> Payload -> Payload
toPayload (SizeUrl bytes) = LBS.take (fromIntegral bytes)
