module SyntheticWeb.Client.Executor
    ( executeTask
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Data.Time (NominalDiffTime)
import SyntheticWeb.Client.Http (get)
import SyntheticWeb.Client.SizeUrl (fromSize)
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
                               , Download (..)
                               , Duration (..) )

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
  ((), timeItTook) <- timedAction $ liftIO (threadDelay $ toDelay duration)
  doUpdateSleepTime timeItTook
    where
      toDelay :: Duration -> Int
      toDelay (Usec t) = t
      toDelay (Msec t) = t * 1000
      toDelay (Sec t)  = t * 1000000

-- | Fetch a resource with the specfied size.
executeActivity (GET headers (Download size) _) = do
  sizeUrl                      <- fromSize size =<< getGenerator
  ((_, byteCount), timeItTook) <- timedAction (get sizeUrl headers)
  doUpdateByteCountAndLatencyTime byteCount timeItTook

executeActivity (PUT _ _) = return ()
executeActivity (POST _ _ _ _) = return ()

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
