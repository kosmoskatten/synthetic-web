{-# LANGUAGE RecordWildCards #-}
module SyntheticWeb.Host
       ( Host (..)
       ) where

import Text.ParserCombinators.ReadP ( ReadP
                                    , char
                                    , skipSpaces
                                    , satisfy
                                    , many
                                    , pfail )
import Text.ParserCombinators.ReadPrec (lift)
import Text.Printf (printf)
import GHC.Read (readPrec)

data Host =
  Host { hostname :: String
       , port     :: Int }
  deriving Eq

instance Read Host where
  readPrec = lift getHost

instance Show Host where
  show Host {..} = printf "%s:%d" hostname port

getHost :: ReadP Host
getHost = do
  hostname' <- getHostname
  skipSpaces ; char ':'
  port'     <- getPort
  return $! Host { hostname = hostname', port = port' }

getHostname :: ReadP String
getHostname = do
  let initSet = ['a'..'z'] ++ ['A'..'Z']
      contSet = initSet ++ ['0'..'9'] ++ "-_."
  skipSpaces
  (:) <$> satisfy (`elem` initSet) <*> many (satisfy (`elem` contSet))

getPort :: ReadP Int
getPort = do
  let initSet = ['1'..'9']
      contSet = '0':initSet
  skipSpaces
  numStr <- (:) <$> satisfy (`elem` initSet) <*> many (satisfy (`elem` contSet))
  case reads numStr of
    [(num, _)] -> return num
    _          -> pfail




