module Parser
    ( readExpression
    ) where

import Data.List ((\\))
import Text.Parsec ((<|>), many, many1, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, oneOf, noneOf, letter, digit, string)

data Value = Symbol String
           | Bool Bool
           | Number Integer
           | String String
           | List [Value]

specialCharacters :: String
specialCharacters = "!$%&*+-./:<=>?@^_~"

specialInitialCharacters :: String
specialInitialCharacters = specialCharacters \\ "+-.@"

specialCharacter :: Parser Char
specialCharacter = oneOf specialCharacters

specialInitialCharacter :: Parser Char
specialInitialCharacter = oneOf specialInitialCharacters

ordinarySymbol :: Parser String
ordinarySymbol = do
  first <- letter <|> specialInitialCharacter
  rest <- many (letter <|> digit <|> specialCharacter)
  return $ first : rest

peculiarSymbol :: Parser String
peculiarSymbol = string "+" <|> string "-" <|> string "..."

symbol :: Parser String
symbol = ordinarySymbol <|> peculiarSymbol

parseSymbol :: Parser Value
parseSymbol = fmap Symbol symbol

boolLiteral :: Parser String
boolLiteral = string "#t" <|> string "#f"

boolValueOf :: String -> Value
boolValueOf "#t" = Bool True
boolValueOf _ = Bool False

parseBool :: Parser Value
parseBool = fmap boolValueOf boolLiteral

digits :: Parser String
digits = many1 digit

numberValueOf :: String -> Value
numberValueOf = Number . read

parseNumber :: Parser Value
parseNumber = fmap numberValueOf digits

escapedCharacter :: Parser Char
escapedCharacter = char '\\' >> oneOf "\"nrt\\"

parseString :: Parser Value
parseString = do
  char '"'
  string <- many (noneOf "\"\\" <|> escapedCharacter)
  char '"'
  return $ String string

parseExpression :: Parser Value
parseExpression = parseSymbol <|> parseBool <|> parseNumber <|> parseString

readExpression :: String -> String
readExpression input = case parse parseExpression "Parser.hs" input of
  Left err -> "No match: " ++ show err
  _ -> "Found value"
