module TestSuite.Client
    ( timedActionShallTimeCorrectly
    ) where

import Control.Concurrent (threadDelay)
import Data.Time ()
import Test.HUnit
import SyntheticWeb.Client.TimedAction (timedAction)

-- | Try a 0.5 s sleep and check that the timed action is withing
-- expected tolerances.
timedActionShallTimeCorrectly :: Assertion
timedActionShallTimeCorrectly = do
  ((), timeItTook) <- timedAction $ threadDelay 500000
  let t   = toEnum 1000000000000 -- One second.
      t'  = t * 0.5
  assertBool "Shall be within range" $ timeItTook >= t' && timeItTook <= t
