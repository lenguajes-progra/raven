module Literal where

import Text.Parsec.String (Parser)
import Control.Applicative hiding (many, optional)
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec.Char as PC

data Literal = IntegerLiteral Integer
             | CharacterLiteral Char
             | StringLiteral String
             | BooleanLiteral Bool
             deriving (Show)

-- Literal Parsers
literalParser :: Parser Literal
literalParser = choice [integerLiteralParser, characterLiteralParser, stringLiteralParser, booleanLiteralParser]

booleanLiteralParser :: Parser Literal
booleanLiteralParser = do
  value <- choice [string "false" >> return False, string "true" >> return True]
  return (BooleanLiteral value)

integerLiteralParser :: Parser Literal
integerLiteralParser = do
  negative <- option False (char '-' >> return True)
  digits <- many1 digit
  let value = read digits :: Integer
  return (if negative then IntegerLiteral (-value) else IntegerLiteral value)

characterLiteralParser :: Parser Literal
characterLiteralParser = do
  char '\''
  value <- PC.letter <|> digit
  char '\''
  return (CharacterLiteral value)

stringLiteralParser :: Parser Literal
stringLiteralParser = do
  char '"'
  value <- many (noneOf "\"")
  char '"'
  return (StringLiteral value)

-- Small Parsers
digitParser :: Parser Char
digitParser = choice [zeroParser, nonZeroDigitParser]

zeroParser :: Parser Char
zeroParser = char '0'

nonZeroDigitParser :: Parser Char
nonZeroDigitParser = oneOf "123456789"

letterParser :: Parser Char
letterParser = letter