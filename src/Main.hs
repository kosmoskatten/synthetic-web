{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs

data SyntheticWeb =
  CmdLine { client :: Maybe String
          , model  :: Maybe FilePath
          , obs    :: Maybe Int            
          , server :: Maybe Int }
  deriving (Show, Data, Typeable)

defHost :: String
defHost = "localhost:38900"

defHostPort :: Int
defHostPort = 38900

defObsPort :: Int
defObsPort = 38901

main :: IO ()
main = print =<< commandLine

commandLine :: IO SyntheticWeb
commandLine = cmdArgs
  CmdLine { client = def &= typ "<HOST>"                                 &=
                     opt defHost &= groupname "Client configuration"     &=
                     help "Run in client mode (default port 38900)"
                     
          , model  = def &= typ "<MODEL FILE PATH>"                      &=
                     help "Model file (client only)"

          , obs    = def &= typ "<OBSERVABILITY PORT>"                   &=
                     opt defObsPort                                      &=
                     help "Observability port (default 38901)"
                     
          , server = def &= typ "<LISTENING PORT>"                       &=
                     opt defHostPort &= groupname "Server configuration" &=
                     help "Run in server mode (default port 38900)"
          }
