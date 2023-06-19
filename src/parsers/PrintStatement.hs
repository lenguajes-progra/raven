module PrintStatement where

import Grammar
import Expression
import Text.Parsec
import Text.Parsec.String

printStatementParser :: Parser PrintStatement
printStatementParser =
  PrintStatement
    <$> (string "print" *> spaces *> parseExpression)
    <* spaces <* char ';'
