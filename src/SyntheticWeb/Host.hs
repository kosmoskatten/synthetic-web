{-# LANGUAGE RecordWildCards #-}
module SyntheticWeb.Host
       ( Host (..)
       , fromString
       ) where

import Control.Applicative ((<$>), (<*>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Char8 as BS
import Text.Printf (printf)

data Host =
  Host { hostname :: !String
       , port     :: !Int }
  deriving Eq

instance Show Host where
  show Host {..} = printf "%s:%d" hostname port

fromString :: String -> Maybe Host
fromString str =
  case expand $ parse parser (BS.pack str) of
    Done _ host -> Just host
    _           -> Nothing
  where
    expand :: Result Host -> Result Host
    expand (Partial cont) = expand $ cont BS.empty
    expand result         = result

parser :: Parser Host
parser = do
  skipSpace ; hostname' <- BS.unpack <$> parseHostname
  skipSpace ; char ':'
  skipSpace ; port'     <- decimal
  skipSpace ; endOfInput
  return Host { hostname = hostname'
              , port     = port' }

parseHostname :: Parser BS.ByteString
parseHostname = do
  let initCharSet = ['a'..'z'] ++ ['A'..'Z']
      contCharSet = initCharSet ++ ['0'..'9'] ++ ['-', '.']
  BS.cons <$> satisfy (`elem` initCharSet)
          <*> AP.takeWhile (`elem` contCharSet)




