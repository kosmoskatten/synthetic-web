module SyntheticWeb.Client.SizeUrl
    ( SizeUrl
    , Payload
    , Url
    , fromSize
    , toUrl
    , toPayload
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import SyntheticWeb.Plan.Types (Bytes, Size (..))
import System.Random.MWC (GenIO, uniformR)
import System.Random.MWC.Distributions (normal)
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
fromSize (Exactly bytes) _          = return $ SizeUrl bytes
fromSize (Uniform range) gen        = SizeUrl <$> liftIO (uniformR range gen)
fromSize (Gauss (mean, stddev)) gen = 
    SizeUrl . truncate <$> 
            liftIO (normal (fromIntegral mean) (fromIntegral stddev) gen)

-- | Translate the SizeUrl to an url.
toUrl :: SizeUrl -> Url
toUrl (SizeUrl bytes) = '/' `BS.cons` (BS.pack $ show bytes)

-- | Translate the SizeUrl to an equally sized payload.
toPayload :: SizeUrl -> Payload -> Payload
toPayload (SizeUrl bytes) = LBS.take (fromIntegral bytes)
