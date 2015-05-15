module SyntheticWeb.Client.ByteCountedConnection
    ( openByteCountedConnection
    , withByteCountedConnection
    ) where

import Control.Exception (bracket)
import Network.Http.Client ( Connection
                           , Hostname
                           , Port
                           , closeConnection
                           , connectSocket
                           , makeConnection
                           , modifyHostname )
import Network.Socket (close)
import SyntheticWeb.Counter.ByteCounter (ByteCounter (..))
import qualified System.IO.Streams as Streams
import GHC.Int (Int64)

type ByteCountedConnection = (Connection, IO Int64, IO Int64)

-- | Open a byte counted connection. Before the connection is closed
-- the number of bytes for download and upload can be read from the IO
-- actions.
openByteCountedConnection :: Hostname -> Port -> IO ByteCountedConnection
openByteCountedConnection host port = do
  sock         <- connectSocket host port return
  streams      <- Streams.socketToStreams sock
  (ins, inc)   <- Streams.countInput (fst streams)
  (outs, outc) <- Streams.countOutput (snd streams)
  conn         <- makeConnection (modifyHostname host port) (close sock)
                                 outs ins
  return (conn, inc, outc)

-- | Execute an action using the byte counted connection. Return the
-- result from the action and a byte counter with the count.
withByteCountedConnection :: IO ByteCountedConnection 
                          -> (Connection -> IO a) 
                          -> IO (a, ByteCounter)
withByteCountedConnection mkConnection action =
    bracket mkConnection
            (\(conn, _, _) -> closeConnection conn)
            (\(conn, inc, outc) -> do
               res <- action conn
               cnt <- ByteCounter <$> inc <*> outc
               return (res, cnt))
