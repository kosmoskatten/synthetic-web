{-# LANGUAGE OverloadedStrings #-}
module SyntheticWeb.Plan.Parser (parsePlan) where

import Control.Applicative ((<$>), (<*>), (*>), (<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import SyntheticWeb.Plan.Types ( Plan
                               , Bytes
                               , Weight (..)
                               , Rate (..)
                               , Activity (..)
                               , Duration (..)
                               , Pattern (..)
                               , Payload (..)
                               , Size (..)
                               , Header (..)
                               )

parsePlan :: Parser Plan
parsePlan = do
  plan <- many' parsePattern
  skipSpace ; endOfInput
  return plan

parsePattern :: Parser (Weight, Pattern)
parsePattern = do
  skipSpace ; string "pattern"
  patternName  <- parseName
  weight       <- parseWeight
  activities'  <- parseList parseActivity
  return (weight, Pattern patternName activities')

parseWeight :: Parser Weight
parseWeight = do
  skipSpace ; string "with" ; skipSpace ; string "weight" ; skipSpace
  Weight <$> decimal

parseName :: Parser String
parseName = do
  skipSpace
  let charSet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_', '-']
  BS.unpack <$> takeWhile1 (`elem` charSet)

parseActivity :: Parser Activity
parseActivity   = parseSleep <|> parseGet <|> parsePut
  where
    parseSleep  =
      SLEEP <$> ((skipSpace >> string "SLEEP") *> parseDuration)
    parseGet    = do
      skipSpace ; string "GET"
      GET <$> parseHeaders <*> parsePayload <*> parseRate
    parsePut    = do
      skipSpace ; string "PUT"
      PUT <$> parseHeaders <*> parsePayload <*> parseRate

parseDuration :: Parser Duration
parseDuration = do
  skipSpace ; duration <- decimal
  skipSpace ; unit     <- parseUnit
  return $ unit duration
  where
    parseUnit = (string "us" *> return Us)
                <|> (string "ms" *> return Ms)
                <|> (string "s" *> return S)

parseHeaders :: Parser [Header]
parseHeaders = do
  skipSpace ; string "headers"
  skipSpace *> parseList parseHeader

parseRate :: Parser Rate
parseRate = do
  skipSpace ; string "rate"
  parseUnlimited <|> parseLimited
  where
    parseUnlimited = do
      skipSpace ; string "unlimited"
      return Unlimited
      
    parseLimited = do
      skipSpace
      string "limitedTo"
      LimitedTo <$> (skipSpace *> parseSize)

parsePayload :: Parser Payload
parsePayload = do
  skipSpace ; string "payload"
  Payload <$> parseSize

parseSize :: Parser Size
parseSize =
  parseExactly
  <|> parseDistribution "uniform" Uniform
  <|> parseDistribution "gauss" Gauss
  where
    parseExactly = do
      skipSpace
      string "exactly"
      Exactly <$> (skipSpace *> decimal)

    parseDistribution :: BS.ByteString -> ((Bytes, Bytes) -> Size)
                         -> Parser Size
    parseDistribution dist ctor = do
      skipSpace
      string dist
      minValue <- skipSpace *> decimal
      skipSpace ; char '-'
      maxValue <- skipSpace *> decimal
      return $ ctor (minValue, maxValue)

parseList :: Parser a -> Parser [a]
parseList parser = do
  skipSpace ; char '['
  items <- parseList' <|> return []
  skipSpace ; char ']'
  return items
  where
    parseList' = do
      skipSpace
      first <- parser
      theRest <- many' $ (skipSpace >> char ',' >> skipSpace) *> parser
      return (first:theRest)

parseHeader :: Parser Header
parseHeader =
  read . BS.unpack <$> headers
  where
    headers =
      string "AcceptAny"
      <|> string "AcceptTextHtml"
      <|> string "AcceptTextPlain"
      <|> string "AcceptApplicationJSON"
      <|> string "ContentTextHtml"
      <|> string "ContentTextPlain"
      <|> string "ContentApplicationJSON"
