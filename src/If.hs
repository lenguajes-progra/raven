module If where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, char, letter, digit, spaces)
import Text.Parsec.Expr (buildExpressionParser, Operator(..))
import Control.Applicative ((<|>))

data Expression = -- Define your Expression data type here

data Block = -- Define your Block data type here

data IfStatement = IfStatement Expression Block Block

ifStatementParser :: Parser IfStatement
ifStatementParser =
  IfStatement
    <$> (string "if" *> spaces *> char '(' *> expressionParser <* char ')')
    <*> (spaces *> blockParser)
    <*> (spaces *> string "else" *> spaces *> blockParser)
    <* spaces
    <* string "end"