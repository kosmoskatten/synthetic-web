module SyntheticWeb.Client.Executor
    ( executeTask
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Data.Time (NominalDiffTime)
import SyntheticWeb.Client.Http (get, post, put)
import SyntheticWeb.Client.TimedAction (timedAction)
import SyntheticWeb.Counter ( ByteCounter
                            , activatePattern
                            , updateByteCount
                            , updateLatencyTime
                            , updatePatternTime
                            , updateSleepTime )
import SyntheticWeb.Client.ExecM ( ExecM
                                 , getCounters
                                 , getActivities
                                 , getGenerator
                                 , liftIO )
import SyntheticWeb.Plan.Types ( Activity (..)
                               , Duration (..) )
import SyntheticWeb.Statistical (Statistical (Exactly), sample)

-- | Execute one task.
executeTask :: ExecM ()
executeTask = do
  ((), timeItTook) <- 
      timedAction $ do
        doActivatePattern
        mapM_ executeActivity =<< getActivities
  doUpdatePatternTime timeItTook

-- | Execute one client activity. Counters related to the activity -
-- timings and byte counters - shall be updated.
executeActivity :: Activity -> ExecM ()

-- | Delay the task worker thread for the specified duration.
executeActivity (SLEEP duration) = do
  delay <- sampleDelayTime duration
  ((), timeItTook) <- timedAction $ liftIO (threadDelay delay)
  doUpdateSleepTime timeItTook

-- | Fetch a resource with the specfied size.
executeActivity (GET headers download _) = do
  ((_, byteCount), timeItTook) <- timedAction (get download headers)
  doUpdateByteCountAndLatencyTime byteCount timeItTook

-- | Upload to a resource with the specified size.
executeActivity (PUT headers upload) = do
  ((_, byteCount), timeItTook) <- timedAction (put upload headers)
  doUpdateByteCountAndLatencyTime byteCount timeItTook

-- | Perform a post (upload and download) with the specicied sizes.
executeActivity (POST headers upload download _) = do
  ((_, byteCount), timeItTook) <- timedAction (post upload download headers)
  doUpdateByteCountAndLatencyTime byteCount timeItTook

sampleDelayTime :: Duration -> ExecM Int
sampleDelayTime (Usec stat) = do
  Exactly t <- sample stat =<< getGenerator
  return t
sampleDelayTime (Msec stat) = do
  Exactly t <- sample stat =<< getGenerator
  return (t * 1000)
sampleDelayTime (Sec stat) = do
  Exactly t <- sample stat =<< getGenerator
  return (t * 1000000)

doActivatePattern :: ExecM ()
doActivatePattern = do
  c <- getCounters
  liftIO $ atomically (activatePattern c)

doUpdateByteCountAndLatencyTime :: ByteCounter -> NominalDiffTime -> ExecM ()
doUpdateByteCountAndLatencyTime bc t = do
  c <- getCounters
  liftIO $ atomically $ do
    updateByteCount bc c
    updateLatencyTime t c

doUpdatePatternTime :: NominalDiffTime -> ExecM ()
doUpdatePatternTime t = do
  c <- getCounters
  liftIO $ atomically (updatePatternTime t c)

doUpdateSleepTime :: NominalDiffTime -> ExecM ()
doUpdateSleepTime t = do
  c <- getCounters
  liftIO $ atomically (updateSleepTime t c)
