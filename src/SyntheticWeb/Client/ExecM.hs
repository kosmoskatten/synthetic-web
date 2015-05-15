{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SyntheticWeb.Client.ExecM
    ( ExecM
    , runExecM
    , getCounters
    , getActivities
    , getGenerator
    , getHost
    , liftIO
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import SyntheticWeb.Counter (CounterPair)
import SyntheticWeb.Host (Host)
import SyntheticWeb.Plan.Types (Activity, Pattern (activities))
import SyntheticWeb.Task (Task, counterFrom, patternFrom)
import System.Random.MWC (GenIO)

data ExecKit =
    ExecKit { _counters   :: !CounterPair
            , _activities :: ![Activity]
            , _generator  :: !GenIO
            , _host       :: !Host }

newtype ExecM a = 
    ExecM { extractExecM :: ReaderT ExecKit IO a }
    deriving (Functor, Applicative, Monad, MonadReader ExecKit, MonadIO)

runExecM :: Task -> GenIO -> Host -> ExecM a -> IO a
runExecM task gen host exec = do
  let kit = ExecKit { _counters   = counterFrom task
                    , _activities = activities (patternFrom task)
                    , _generator  = gen
                    , _host       = host }
  runReaderT (extractExecM exec) kit

getCounters :: ExecM CounterPair
getCounters = _counters <$> ask

getActivities :: ExecM [Activity]
getActivities = _activities <$> ask

getGenerator :: ExecM GenIO
getGenerator = _generator <$> ask

getHost :: ExecM Host
getHost = _host <$> ask
          
