module SyntheticWeb.Plan.Parser2 where

import SyntheticWeb.Plan.Types
import Text.Parsec.ByteString
import Text.Parsec

parsePlan2 :: Parser Plan
parsePlan2 = return $ Plan []

parsePattern :: Parser (Weight, Pattern)
parsePattern = do
  spaces ; string "pattern"
  return (Weight 1, Pattern "" [])

parseWeight :: Parser Weight
parseWeight = do
  spaces ; string "with" ; spaces ; string "weight"
  spaces ; Weight . read <$> many1 digit

parseName :: Parser String
parseName = do
  let initSet = ['a'..'z'] ++ ['A'..'Z']
      contSet = initSet ++ ['0'..'9'] ++ "-_."
  spaces ; (:) <$> oneOf initSet <*> many (oneOf contSet)

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
      headers = try (string "AcceptAny") 
                <|> try (string "AcceptTextHtml")
                <|> try (string "AcceptTextPlain")
                <|> try (string "AcceptApplicationJSON")
                <|> try (string "ContentTextHtml")
                <|> try (string "ContentTextPlain")
                <|> try (string "ContentApplicatonJSON")

parseListOf :: Parser a -> Parser [a]
parseListOf p = do
  spaces ; char '['
  list <- try parseListOf' <|> return []
  spaces ; char ']'
  return list
    where
      parseListOf' = (:) <$> p <*> many (spaces >> char ',' *> p)
