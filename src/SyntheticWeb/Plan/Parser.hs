module SyntheticWeb.Plan.Parser (parsePlan) where

import Control.Monad (void)
import SyntheticWeb.Plan.Types ( Plan (..)
                               , Weight (..)
                               , Rate (..)
                               , Activity (..)
                               , Duration (..)
                               , Pattern (..)
                               , Download (..)
                               , Upload (..)
                               , Size (..)
                               , Header (..)
                               )
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer =
  Token.makeTokenParser emptyDef
  { Token.commentLine   = "#"
  , Token.identStart    = letter
  , Token.identLetter   = alphaNum <|> oneOf "-_."
  , Token.reservedNames = [ "pattern", "with", "weight", "download"
                          , "upload", "get", "put", "post", "sleep"
                          , "headers" , "rate", "limitedto"
                          , "unlimited" , "gauss", "uniform", "exactly"
                          , "accept-any", "accept-text-html"
                          , "accept-text-plain", "accept-application-json"
                          , "content-text-html", "content-text-plain"
                          , "content-application-json", "usec", "msec", "sec" ]
  , Token.caseSensitive = False
  }

parsePlan :: Parser Plan
parsePlan = do
  whiteSpace
  patterns <- many (lexeme pattern)
  eof
  return $ Plan patterns
  
pattern :: Parser (Weight, Pattern)
pattern = do
  reserved "pattern"
  name'        <- identifier
  weight       <- withWeight
  activities'' <- activities'
  return $ (weight, Pattern name' activities'')

withWeight :: Parser Weight
withWeight = do
  reserved "with"
  reserved "weight"
  Weight <$> decimal

activities' :: Parser [Activity]
activities' = brackets $ commaSep activity

activity :: Parser Activity
activity = get <|> post <|> put <|> sleep
  where
    get = do
      reserved "get"
      GET <$> headers <*> download <*> rate
    post = do
      reserved "post"
      POST <$> headers <*> upload <*> download <*> rate
    put = do
      reserved "put"
      PUT <$> headers <*> upload
    sleep = do
      reserved "sleep"
      SLEEP <$> duration

download :: Parser Download
download = do
  reserved "download"
  Download <$> size

upload :: Parser Upload
upload = do
  reserved "Upload"
  Upload <$> size

rate :: Parser Rate
rate = do
  reserved "rate"
  unlimited <|> limitedTo
  where
    unlimited = do
      reserved "unlimited"
      return Unlimited
    limitedTo = do
      reserved "limitedTo"
      LimitedTo <$> size

size :: Parser Size
size = exactly <|> gauss <|> uniform
  where
    exactly = do
      reserved "exactly"
      Exactly <$> decimal
    gauss = do
      reserved "gauss"
      curry Gauss <$> decimal <*> (char' '-' *> decimal)
    uniform = do
      reserved "uniform"
      curry Uniform <$> decimal <*> (char' '-' *> decimal)

headers :: Parser [Header]
headers = do
  reserved "headers"
  brackets $ commaSep header

header :: Parser Header
header = reserved "accept-any" *> pure AcceptAny
         <|> reserved "accept-text-html" *> pure AcceptTextHtml
         <|> reserved "accept-text-plain" *> pure AcceptTextPlain
         <|> reserved "accept-application-json" *> pure AcceptApplicationJSON
         <|> reserved "content-text-html" *> pure ContentTextHtml
         <|> reserved "content-text-plain" *> pure ContentTextPlain
         <|> reserved "content-application-json" *> pure ContentApplicationJSON

duration :: Parser Duration
duration = do
  t <- decimal
  u <- unit
  return $ u t
  where
    unit = (reserved "usec" >> pure Usec)
           <|> (reserved "msec" >> pure Msec)
           <|> (reserved "sec" >> pure Sec)

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

brackets :: Parser a -> Parser a
brackets p = lexeme $ Token.brackets lexer p

commaSep :: Parser a -> Parser [a]
commaSep p = lexeme $ Token.commaSep lexer p

identifier :: Parser String
identifier = lexeme $ Token.identifier lexer

decimal :: Parser Int
decimal = fromInteger <$> (lexeme $ Token.decimal lexer)

reserved :: String -> Parser ()
reserved str = lexeme $ Token.reserved lexer str

char' :: Char -> Parser ()
char' = lexeme . void . char

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
