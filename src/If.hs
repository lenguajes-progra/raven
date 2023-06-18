module If where

import Text.Parsec
import Text.Parsec.String (Parser)
import Expression (Expression, parseExpression)

data IfStatement = IfStatement Expression Block Block deriving (Show)

type Block = String 

ifStatementParser :: Parser IfStatement
ifStatementParser =
  IfStatement
    <$> (string "if" *> spaces *> char '(' *> parseExpression <* char ')')
    <*> (spaces *> string "block")
    <*> (spaces *> string "else" *> spaces *> string "block")
    <* spaces
    <* string "end"