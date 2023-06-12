module Type where

import Text.Parsec
import Text.Parsec.String (Parser)

data Identifier = Identifier String
  deriving (Show)

identifierParser :: Parser Identifier
identifierParser = do
  first <- letter
  rest <- many (letter <|> digit <|> char '_')
  return (Identifier (first:rest))

parseIdentifier :: String -> Either ParseError Identifier
parseIdentifier input = parse identifierParser "" input

