module SyntheticWeb.Client.TimedAction
    ( timedAction
    ) where

import Control.DeepSeq (force)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time ( NominalDiffTime
                 , getCurrentTime
                 , diffUTCTime )

-- | Execute an IO action and return the action's value together with
-- the time it took to execute the action.
timedAction :: MonadIO m => m a -> m (a, NominalDiffTime)
timedAction act = do
  start <- force <$> liftIO getCurrentTime
  value <- act
  stop  <- force <$> liftIO getCurrentTime
  return (value, stop `diffUTCTime` start)
