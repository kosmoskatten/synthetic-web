{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SyntheticWeb.Client.ExecM
    ( ExecM
    , runExecM
    , getCounters
    , getActivities
    , getGenerator
    , getHost
    , getPayload
    , liftIO
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Data.ByteString.Lazy.Char8 (ByteString)
import SyntheticWeb.Counter (CounterPair)
import SyntheticWeb.Host (Host)
import SyntheticWeb.Plan.Types (Activity, Pattern (activities))
import SyntheticWeb.Task (Task, counterFrom, patternFrom)
import System.Random.MWC (GenIO)

data ExecKit =
    ExecKit { _counters   :: !CounterPair
            , _activities :: ![Activity]
            , _generator  :: !GenIO
            , _host       :: !Host
            , _payload    :: !ByteString }

newtype ExecM a = 
    ExecM { extractExecM :: ReaderT ExecKit IO a }
    deriving (Functor, Applicative, Monad, MonadReader ExecKit, MonadIO)

runExecM :: Task -> GenIO -> Host -> ByteString -> ExecM a -> IO a
runExecM task gen host payload exec = do
  let kit = ExecKit { _counters   = counterFrom task
                    , _activities = activities (patternFrom task)
                    , _generator  = gen
                    , _host       = host
                    , _payload    = payload }
  runReaderT (extractExecM exec) kit

getCounters :: ExecM CounterPair
getCounters = _counters <$> ask

getActivities :: ExecM [Activity]
getActivities = _activities <$> ask

getGenerator :: ExecM GenIO
getGenerator = _generator <$> ask

getHost :: ExecM Host
getHost = _host <$> ask

getPayload :: ExecM ByteString
getPayload = _payload <$> ask
          
