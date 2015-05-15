module SyntheticWeb.Client.Http
    ( get
    ) where

import Network.Http.Client ( Method (..)
                           , StatusCode 
                           , buildRequest
                           , http
                           , emptyBody
                           , getStatusCode
                           , receiveResponse
                           , sendRequest )
import SyntheticWeb.Client.ByteCountedConnection ( openByteCountedConnection
                                                 , withByteCountedConnection )
import SyntheticWeb.Client.ExecM ( ExecM
                                 , getHost
                                 , liftIO )
import SyntheticWeb.Client.SizeUrl (SizeUrl, toBS)
import SyntheticWeb.Counter.ByteCounter (ByteCounter)
import SyntheticWeb.Host (Host (..))
import SyntheticWeb.Plan.Types (Header)
import qualified System.IO.Streams as Streams

get :: SizeUrl -> [Header] -> ExecM (StatusCode, ByteCounter)
get sizeUrl headers = do
  host <- getHost
  liftIO $
    withByteCountedConnection 
      (openByteCountedConnection (hostname host) (port host)) $ \conn -> do
          let request = buildRequest $ do
                          http GET (toBS sizeUrl)

          sendRequest conn request emptyBody
          receiveResponse conn $ \resp inp -> do
                        Streams.skipToEof inp
                        return (getStatusCode resp)
    
