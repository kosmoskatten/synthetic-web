{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module SyntheticWeb.Statistical
    ( Statistical (..)
    , sample
    ) where

import Control.DeepSeq (NFData ())
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Generics (Generic)
import System.Random.MWC (GenIO, Variate (), uniformR)
import System.Random.MWC.Distributions (normal)

-- | A statistical data structure which value can be 'sampled'. All
-- samples will be constructed by Exactly.
data Statistical a =
      Exactly !a
      -- ^ A sample will yield 'Exactly a'.
    | Gaussian !(a, a)
      -- ^ A sample will yield a value described by the gaussian
      -- distribution of (mean, stddev).
    | Uniform !(a, a)
    -- ^ A sample will yield a value described by the uniform
    -- distribution of (start range, end range).
    deriving (Eq, Generic, NFData, Show)

-- | Sample a statistical value. The value yielded is always
-- constructed with Exactly and its primitive value is never negative.
sample :: (Integral a, Variate a, MonadIO m) => 
          Statistical a -> GenIO -> m (Statistical a)
sample (Gaussian (mean, stddev)) gen = 
    Exactly . atlz . truncate <$> 
            liftIO (normal (realToFrac mean) (realToFrac stddev) gen)
sample (Uniform range) gen           = 
    Exactly . atlz <$> liftIO (uniformR range gen)
sample (Exactly x) _                 = Exactly . atlz <$> pure x

-- | Evaluate to at least zero.
atlz :: (Num a, Ord a) => a -> a
atlz x
    | signum x > 0 = x
    | otherwise    = x + abs x

