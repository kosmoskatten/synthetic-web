module SyntheticWeb.Plan.Parser2 (parsePlan2) where

import SyntheticWeb.Plan.Types ( Plan (..)
                               , Weight (..)
                               , Rate (..)
                               , Activity (..)
                               , Duration (..)
                               , Pattern (..)
                               , Payload (..)
                               , Size (..)
                               , Header (..)
                               )
import Text.Parsec.ByteString
import Text.Parsec

parsePlan2 :: Parser Plan
parsePlan2 = do
  patterns <- manyTill parsePattern (try eof)
  return $ Plan patterns

parsePattern :: Parser (Weight, Pattern)
parsePattern = do
  spaces ; string "pattern"
  name'       <- parseName
  weight'     <- parseWeight
  activities' <- listOf parseActivity
  return (weight', Pattern name' activities')

parseWeight :: Parser Weight
parseWeight = do
  spaces ; string "with" ; spaces ; string "weight"
  spaces ; Weight . read <$> many1 digit

parseName :: Parser String
parseName = do
  let initSet = ['a'..'z'] ++ ['A'..'Z']
      contSet = initSet ++ ['0'..'9'] ++ "-_."
  spaces ; (:) <$> oneOf initSet <*> many (oneOf contSet)

parseActivity :: Parser Activity
parseActivity =
  try parseGet <|> try parsePut <|> try parseSleep
  where
    parseGet = do
      spaces ; string "GET"
      GET <$> parseHeaders <*> parsePayload <*> parseRate
    parsePut = do
      spaces ; string "PUT"
      PUT <$> parseHeaders <*> parsePayload <*> parseRate
    parseSleep = do
      spaces ; string "SLEEP"
      SLEEP <$> parseDuration

parseDuration :: Parser Duration
parseDuration = do
  spaces ; duration <- read <$> many1 digit
  unit <- try parseUs <|> try parseMs <|> try parseS
  return $ unit duration
  where
    parseUs = do
      spaces ; string "us" ; return Us
    parseMs = do
      spaces ; string "ms" ; return Ms
    parseS  = do
      spaces ; string "s" ; return S

parseHeaders :: Parser [Header]
parseHeaders = do
  spaces ; string "headers"
  listOf parseHeader

parseRate :: Parser Rate
parseRate = do
  spaces ; string "rate"
  (try parseUnlimited) <|> (try parseLimitedTo)
  where
    parseUnlimited = do
      spaces ; string "unlimited" ; return Unlimited
    parseLimitedTo = do
      spaces ; string "limitedTo"
      LimitedTo <$> parseSize

parsePayload :: Parser Payload
parsePayload = do
  spaces ; string "payload"
  Payload <$> parseSize

parseSize :: Parser Size
parseSize = (try parseExactly)
            <|> (try $ parseDist "gauss" Gauss)
            <|> (try $ parseDist "uniform" Uniform)
  where
    parseExactly :: Parser Size
    parseExactly = do
      spaces ; string "exactly"
      spaces ; Exactly . read <$> many1 digit

    parseDist :: String -> ((Int, Int) -> Size) -> Parser Size
    parseDist dist ctor = do
      spaces ; string dist
      spaces ; minV <- read <$> many1 digit
      spaces ; char '-'
      spaces ; maxV <- read <$> many1 digit
      return $ ctor (minV, maxV)

parseHeader :: Parser Header
parseHeader = do
  spaces ; read <$> headers
    where
      headers =
        choice [ try $ string "AcceptAny"
               , try $ string "AcceptTextHtml"
               , try $ string "AcceptTextPlain"
               , try $ string "AcceptApplicationJSON"
               , try $ string "ContentTextHtml"
               , try $ string "ContentTextPlain"
               , try $ string "ContentApplicationJSON"
               ]

listOf :: Parser a -> Parser [a]
listOf p = between (spaces >> char '[') (char ']') $
                   choice [ try $ (p <* spaces) `sepBy1` (char ',')
                          , spaces *> return []
                          ]