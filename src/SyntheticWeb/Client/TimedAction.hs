module SyntheticWeb.Client.TimedAction
    ( timedAction
    ) where

import Control.DeepSeq (force)
import Data.Time ( NominalDiffTime
                 , getCurrentTime
                 , diffUTCTime )

-- | Execute an IO action and return the action's value together with
-- the time it took to execute the action.
timedAction :: IO a -> IO (a, NominalDiffTime)
timedAction act = do
  start <- force <$> getCurrentTime
  value <- act
  stop <- force <$> getCurrentTime
  return (value, stop `diffUTCTime` start)
