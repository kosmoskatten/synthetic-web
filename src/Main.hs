{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent.Async
import Control.Monad (void, when)
import Control.Monad.Writer (execWriter, tell)
import Control.Monad.State (execState, modify)
import Data.Maybe (isNothing, isJust, fromJust)
import SyntheticWeb.Plan (Plan, parsePlan, parsePlan2)
import SyntheticWeb.Host (Host (..))
import qualified SyntheticWeb.Client as Client
import qualified SyntheticWeb.Observer as Observer
import qualified SyntheticWeb.Server as Server
import System.Console.CmdArgs
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams
import Text.Parsec (ParseError)
import Text.Parsec.ByteString (parseFromFile)
import Text.Printf (printf)

data SyntheticWeb =
  CmdLine { client   :: Maybe String
          , model    :: Maybe FilePath
          , observer :: Maybe Int 
          , server   :: Maybe Int }
  deriving (Show, Data, Typeable)

main :: IO ()
main = do
  services <- mkServices =<< commandLine
  void $ waitAnyCancel =<< mapM async services

defHost :: String
defHost = printf "localhost:%d" defHostPort

defHostPort :: Int
defHostPort = 22000

defObsPort :: Int
defObsPort = 22001

commandLine :: IO SyntheticWeb
commandLine = cmdArgs
  CmdLine { client   = def &= typ "<HOST:PORT>"                            &=
                       opt defHost &= groupname "Client configuration"     &=
                       help helpStrClient
                     
          , model    = def &= typ "<MODEL FILE PATH>"                      &=
                       help "Model file path"

          , observer = def &= typ "<OBSERVABILITY PORT>"                   &=
                       opt defObsPort                                      &=
                       help helpStrObs
                     
          , server   = def &= typ "<LISTENING PORT>"                       &=
                       opt defHostPort &= groupname "Server configuration" &=
                       help helpStrServer
          }
  where
    helpStrClient = printf "Start client module (default connecting to %s)"
                           defHost
    helpStrObs    = printf "Start obs module (default at %d)" defObsPort
    helpStrServer = printf "Start server module (default port %d)" defHostPort

mkServices :: SyntheticWeb -> IO [IO ()]
mkServices cmdLine@CmdLine {..}
  | isJust client && isNothing model =
    error "A model file path must be given. See --help option"
  | isNothing client && isNothing server =
    error "At least one of client or server must be started. See --help option"
  | otherwise = return $ prepareServices cmdLine

prepareServices :: SyntheticWeb -> [IO ()]
prepareServices CmdLine {..} =
  flip execState [] $ do
    when (isJust client) $ modify ((:) Client.service)
    when (isJust observer) $ modify ((:) Observer.service)
    when (isJust server) $ modify ((:) (Server.service $ fromJust server))

readPlanFromFile :: FilePath -> IO Plan
readPlanFromFile filePath = 
    Streams.withFileAsInput filePath $ Streams.parseFromStream parsePlan

readPlanFromFile2 :: FilePath -> IO (Either ParseError Plan)
readPlanFromFile2 = parseFromFile parsePlan2
