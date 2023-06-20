module If where

import Expression
import Grammar
import Statement (blockParse)
import Text.Parsec
import Text.Parsec.String (Parser)

ifStatementParser :: Parser IfStatement
ifStatementParser =
  IfStatement
    <$> (string "if" *> spaces *> char '(' *> parseExpression <* char ')')
    <*> (spaces *> blockParse)
    <*> (spaces *> string "else" *> spaces *> blockParse)
    <* spaces
    <* string "end"