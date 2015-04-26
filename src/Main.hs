{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent.Async
import Control.DeepSeq (force)
import Control.Exception (Exception, evaluate, throw)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Writer (execWriter, tell)
import Control.Monad.State (execStateT, modify)
import Data.Maybe (isNothing, isJust, fromMaybe, fromJust)
import SyntheticWeb.Plan (Plan, parsePlan)
import SyntheticWeb.Host (Host (..))
import qualified SyntheticWeb.Client as Client
import qualified SyntheticWeb.Observer as Observer
import qualified SyntheticWeb.Server as Server
import System.Console.CmdArgs
import Text.Parsec (ParseError)
import Text.Parsec.ByteString (parseFromFile)
import Text.Printf (printf)

type Service = IO ()

data ParseException =
  ParseException { parseError :: !ParseError }
  deriving (Show, Typeable)

instance Exception ParseException where

data SyntheticWeb =
  CmdLine { client   :: Maybe String
          , model    :: Maybe FilePath
          , workers  :: Maybe Int
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

defWorkers :: Int
defWorkers = 1

defObsPort :: Int
defObsPort = 22001

commandLine :: IO SyntheticWeb
commandLine = cmdArgs
  CmdLine { client   = def &= typ "<HOST:PORT>"                            &=
                       opt defHost &= groupname "Client configuration"     &=
                       help helpStrClient
                     
          , model    = def &= typ "<MODEL FILE PATH>"                      &=
                       help "Model file path"

          , workers  = def &= typ "<NUM OF CONCURRENT PATTERNS>"           &=
                       opt defWorkers                                      &=
                       help helpWorkers

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
    helpWorkers   = printf "Number of concurrent patterns (default %d)"
                           defWorkers
    helpStrObs    = printf "Start obs module (default at %d)" defObsPort
    helpStrServer = printf "Start server module (default port %d)" defHostPort

mkServices :: SyntheticWeb -> IO [IO ()]
mkServices cmdLine@CmdLine {..}
  | isJust client && isNothing model =
    error "A model file path must be given. See --help option"
  | isNothing client && isNothing server =
    error "At least one of client or server must be started. See --help option"
  | otherwise = prepareServices cmdLine

prepareServices :: SyntheticWeb -> IO [Service]
prepareServices CmdLine {..} =
  flip execStateT [] $ do
    when (isJust client) $ do
      plan <- either (throw . ParseException) id <$>
               liftIO (readPlanFromFile (fromJust model))
      -- Force exceptions, if any, before starting services.
      plan' <- liftIO $ evaluate $ force plan
      modify ((:) $ Client.service (fromMaybe defWorkers workers) plan')
      
    when (isJust observer) $ modify ((:) Observer.service)
    when (isJust server) $ modify ((:) (Server.service $ fromJust server))

readPlanFromFile :: FilePath -> IO (Either ParseError Plan)
readPlanFromFile = parseFromFile parsePlan
