module SyntheticWeb.Client
       ( service
       ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

service :: IO ()
service =
  forever $ do
    putStrLn "Client"
    threadDelay 1000000
