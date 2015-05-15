module SyntheticWeb.Client.SizeUrl
    ( SizeUrl
    , fromSize
    , toBS
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import SyntheticWeb.Plan.Types (Bytes, Size (..))
import System.Random.MWC (GenIO, uniformR)
import System.Random.MWC.Distributions (normal)
import qualified Data.ByteString.Char8 as BS

-- | Representation of a "size url". An url where a number - integer
-- based size - shall be translated to an url.
newtype SizeUrl = SizeUrl Bytes
  deriving (Show)

-- | Create a SizeUrl from a Size.
fromSize :: MonadIO m => Size -> GenIO -> m SizeUrl
fromSize (Exactly bytes) _          = return $ SizeUrl bytes
fromSize (Uniform range) gen        = SizeUrl <$> liftIO (uniformR range gen)
fromSize (Gauss (mean, stddev)) gen = 
    SizeUrl . truncate <$> 
            liftIO (normal (fromIntegral mean) (fromIntegral stddev) gen)

-- | Translate the SizeUrl to a bytestring.
toBS :: SizeUrl -> BS.ByteString
toBS (SizeUrl bytes) = '/' `BS.cons` (BS.pack $ show bytes)
