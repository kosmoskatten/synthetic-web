module SyntheticWeb.Observer
       ( service
       ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

service :: IO ()
service =
  forever $ do
    putStrLn "Observer"
    threadDelay 1100000
