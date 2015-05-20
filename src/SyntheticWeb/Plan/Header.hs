{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module SyntheticWeb.Plan.Header where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData ())
import GHC.Generics (Generic)
import GHC.Read (readPrec)
import Text.ParserCombinators.ReadP (ReadP, skipSpaces, string)
import Text.ParserCombinators.ReadPrec (lift)

data HeaderM = AcceptTextHtml
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

instance Read HeaderM where
    readPrec = lift readHeader

readHeader :: ReadP HeaderM
readHeader =
      skipString "accept-text/html" *> return AcceptTextHtml
  <|> skipString "accept-text/plain" *> return AcceptTextPlain
  <|> skipString "accept-application/json" *> return AcceptApplicationJson
  <|> skipString "accept-application/xml" *> return AcceptApplicationXml
  <|> skipString "accept-image/jpeg" *> return AcceptImageJpeg
  <|> skipString "accept-video/mpeg" *> return AcceptVideoMpeg
  <|> skipString "accept-audio/mpeg" *> return AcceptAudioMpeg
  <|> skipString "content-text/html" *> return ContentTextHtml
  <|> skipString "content-text/plain" *> return ContentTextPlain
  <|> skipString "content-application/json" *> return ContentApplicationJson
  <|> skipString "content-application/xml" *> return ContentApplicationXml
  <|> skipString "content-image/jpeg" *> return ContentImageJpeg
  <|> skipString "content-video/mpeg" *> return ContentVideoMpeg
  <|> skipString "content-audio/mpeg" *> return ContentAudioMpeg

skipString :: String -> ReadP String
skipString str = skipSpaces >> string str

instance Show HeaderM where
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
