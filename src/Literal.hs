module Literal where

import Text.Parsec
import Text.Parsec.String

data Literal = IntegerLiteral Integer
             | CharacterLiteral Char
             | StringLiteral String
             | BooleanLiteral Bool
             deriving (Show)

literalParser :: Parser Literal
literalParser = integerLiteralParser
            <|> characterLiteralParser
            <|> stringLiteralParser
            <|> booleanLiteralParser

booleanLiteralParser :: Parser Literal
booleanLiteralParser = string "false" *> pure (BooleanLiteral False)
                    <|> string "true" *> pure (BooleanLiteral True)

integerLiteralParser :: Parser Literal
integerLiteralParser = IntegerLiteral . read <$> ((:) <$> (digit <|> char '-') <*> many digit)

characterLiteralParser :: Parser Literal
characterLiteralParser = CharacterLiteral <$> (char '\'' *> (letter <|> digit) <* char '\'')

stringLiteralParser :: Parser Literal
stringLiteralParser = StringLiteral <$> (char '\"' *> many (noneOf "\"") <* char '\"')