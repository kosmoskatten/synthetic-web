module SyntheticWeb.Client.Http
    ( Reply
    , get
    , post
    , put
    ) where

import Blaze.ByteString.Builder (Builder)
import Network.Http.Client ( Method (..)
                           , StatusCode 
                           , buildRequest
                           , http
                           , emptyBody
                           , getStatusCode
                           , inputStreamBody
                           , receiveResponse
                           , sendRequest )
import SyntheticWeb.Client.ByteCountedConnection ( openByteCountedConnection
                                                 , withByteCountedConnection )
import SyntheticWeb.Client.ExecM ( ExecM
                                 , getGenerator
                                 , getHost
                                 , getPayload
                                 , liftIO )
import SyntheticWeb.Client.SizeUrl (SizeUrl, fromSize, toPayload, toUrl)
import SyntheticWeb.Counter.ByteCounter (ByteCounter)
import SyntheticWeb.Host (Host (..))
import SyntheticWeb.Plan.Types (Header, Download (..), Upload (..))
import System.IO.Streams (OutputStream)
import qualified System.IO.Streams as Streams

type Reply = (StatusCode, ByteCounter)

get :: Download -> [Header] -> ExecM Reply
get (Download bytes) headers = do
  sizeUrl <- fromSize bytes =<< getGenerator
  op GET sizeUrl emptyBody headers

post :: Upload -> Download -> [Header] -> ExecM Reply
post (Upload ulBytes) (Download dlBytes) headers = do
  sizeUrlUl <- fromSize ulBytes =<< getGenerator
  sizeUrlDl <- fromSize dlBytes =<< getGenerator
  payload   <- getPayload
  content   <- liftIO $ Streams.fromLazyByteString (toPayload sizeUrlUl payload)
  op POST sizeUrlDl (inputStreamBody content) headers

put :: Upload -> [Header] -> ExecM Reply
put (Upload bytes) headers = do
  sizeUrl <- fromSize bytes =<< getGenerator
  payload <- getPayload
  content <- liftIO $ Streams.fromLazyByteString (toPayload sizeUrl payload)
  op PUT sizeUrl (inputStreamBody content) headers

op :: Method -> SizeUrl -> (OutputStream Builder -> IO ()) 
   -> [Header] -> ExecM Reply
op method url output headers = do
  host <- getHost
  liftIO $
    withByteCountedConnection
      (openByteCountedConnection (hostname host) (port host)) $ \conn -> do
         let request = buildRequest $ do
                         http method (toUrl url)
         sendRequest conn request output
         receiveResponse conn $ \resp inp -> do
           Streams.skipToEof inp
           return (getStatusCode resp)
