module SyntheticWeb.Client
       ( service
       ) where

import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Monad (forever, replicateM, void)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Vector (Vector)
import SyntheticWeb.Client.ExecM (runExecM)
import SyntheticWeb.Client.Executor (executeTask)
import SyntheticWeb.Host (Host)
import SyntheticWeb.RandomData (randomData)
import SyntheticWeb.Task (Task)
import System.Random (randomRIO)
import System.Random.MWC (createSystemRandom)
import qualified Data.Vector as Vector

type TaskSelector = IO Task

-- | Initialize the client service.
service :: Host -> Int -> Vector Task -> IO ()
service host numWorkers tasks = do
  let taskSelector = mkTaskSelector tasks
      payload      = randomData
  void $ waitAnyCancel =<<
    replicateM numWorkers (async $ worker taskSelector host payload)

-- | Make a function that randomly (uniformly distributed) will select
-- a task from the vector of tasks.
mkTaskSelector :: Vector Task -> TaskSelector
mkTaskSelector tasks =
  let len = Vector.length tasks
  in Vector.unsafeIndex tasks <$> randomRIO (0, len - 1)

-- | Worker thread. Forever it will select and execute tasks.
worker :: TaskSelector -> Host -> ByteString -> IO ()
worker select host payload =
  forever $ do
    task <- select
    gen  <- createSystemRandom
    runExecM task gen host payload executeTask
    
