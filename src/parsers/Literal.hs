module Literal where

import Text.Parsec
import Text.Parsec.String
import Grammar


literalParser :: Parser Literal
literalParser = try integerLiteralParser
            <|> try characterLiteralParser
            <|> try stringLiteralParser
            <|> try booleanLiteralParser

booleanLiteralParser :: Parser Literal
booleanLiteralParser = try (string "false" *> pure (BooleanLiteral False))
                    <|> try (string "true" *> pure (BooleanLiteral True))

integerLiteralParser :: Parser Literal
integerLiteralParser = IntegerLiteral . read <$> ((:) <$> (digit <|> char '-') <*> many digit)

characterLiteralParser :: Parser Literal
characterLiteralParser = CharacterLiteral <$> (char '\'' *> (letter <|> digit) <* char '\'')

stringLiteralParser :: Parser Literal
stringLiteralParser = StringLiteral <$> (char '\"' *> many (noneOf "\"") <* char '\"')
