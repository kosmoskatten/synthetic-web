{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module SyntheticWeb.Plan.Header 
    ( Header (..)
    , toTuple
    ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData ())
import GHC.Generics (Generic)
import GHC.Read (readPrec)
import Text.ParserCombinators.ReadP (ReadP, skipSpaces, string)
import Text.ParserCombinators.ReadPrec (lift)
import Data.ByteString.Char8 (ByteString)

data Header = AcceptTextHtml
            | AcceptTextPlain
            | AcceptApplicationJson
            | AcceptApplicationXml
            | AcceptImageJpeg
            | AcceptVideoMpeg
            | AcceptAudioMpeg
            | ContentTextHtml
            | ContentTextPlain
            | ContentApplicationJson
            | ContentApplicationXml
            | ContentImageJpeg
            | ContentVideoMpeg
            | ContentAudioMpeg
  deriving (Bounded, Enum, Eq, Generic, NFData)

-- | Convert a header to a Http header tuple suitable for the
-- http-streams API.
toTuple :: Header -> (ByteString, ByteString)
toTuple AcceptTextHtml         = ("Accept", "text/html")
toTuple AcceptTextPlain        = ("Accept", "text/plain")
toTuple AcceptApplicationJson  = ("Accept", "application/json")
toTuple AcceptApplicationXml   = ("Accept", "application/xml")
toTuple AcceptImageJpeg        = ("Accept", "image/jpeg")
toTuple AcceptVideoMpeg        = ("Accept", "video/mpeg")
toTuple AcceptAudioMpeg        = ("Accept", "audio/mpeg")
toTuple ContentTextHtml        = ("Content-Type", "text/html")
toTuple ContentTextPlain       = ("Content-Type", "text/plain")
toTuple ContentApplicationJson = ("Content-Type", "application/json")
toTuple ContentApplicationXml  = ("Content-Type", "application/xml")
toTuple ContentImageJpeg       = ("Content-Type", "image/jpeg")
toTuple ContentVideoMpeg       = ("Content-Type", "video/mpeg")
toTuple ContentAudioMpeg       = ("Content-Type", "audio/mpeg")

instance Read Header where
    readPrec = lift readHeader

readHeader :: ReadP Header
readHeader =
      skipString "accept-text/html" *> pure AcceptTextHtml
  <|> skipString "accept-text/plain" *> pure AcceptTextPlain
  <|> skipString "accept-application/json" *> pure AcceptApplicationJson
  <|> skipString "accept-application/xml" *> pure AcceptApplicationXml
  <|> skipString "accept-image/jpeg" *> pure AcceptImageJpeg
  <|> skipString "accept-video/mpeg" *> pure AcceptVideoMpeg
  <|> skipString "accept-audio/mpeg" *> pure AcceptAudioMpeg
  <|> skipString "content-text/html" *> pure ContentTextHtml
  <|> skipString "content-text/plain" *> pure ContentTextPlain
  <|> skipString "content-application/json" *> pure ContentApplicationJson
  <|> skipString "content-application/xml" *> pure ContentApplicationXml
  <|> skipString "content-image/jpeg" *> pure ContentImageJpeg
  <|> skipString "content-video/mpeg" *> pure ContentVideoMpeg
  <|> skipString "content-audio/mpeg" *> pure ContentAudioMpeg

skipString :: String -> ReadP String
skipString str = skipSpaces >> string str

instance Show Header where
  show AcceptTextHtml         = "accept-text/html"
  show AcceptTextPlain        = "accept-text/plain"
  show AcceptApplicationJson  = "accept-application/json"
  show AcceptApplicationXml   = "accept-application/xml"
  show AcceptImageJpeg        = "accept-image/jpeg"
  show AcceptVideoMpeg        = "accept-video/mpeg"
  show AcceptAudioMpeg        = "accept-audio/mpeg"
  show ContentTextHtml        = "content-text/html"
  show ContentTextPlain       = "content-text/plain"
  show ContentApplicationJson = "content-application/json"
  show ContentApplicationXml  = "content-application/xml"
  show ContentImageJpeg       = "content-image/jpeg"
  show ContentVideoMpeg       = "content-video/mpeg"
  show ContentAudioMpeg       = "content-audio/mpeg"
