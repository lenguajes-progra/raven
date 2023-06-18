module If where

import Text.Parsec
import Text.Parsec.String (Parser)
import Expression 
import Grammar

ifStatementParser :: Parser IfStatement
ifStatementParser =
  IfStatement
    <$> (string "if" *> spaces *> char '(' *> parseExpression <* char ')')
    <*> (spaces *> string "block")
    <*> (spaces *> string "else" *> spaces *> string "block")
    <* spaces
    <* string "end"