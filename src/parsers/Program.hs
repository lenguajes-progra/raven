module Program where

import Text.Parsec
import Text.Parsec.String
import Grammar
import Statement

programParser :: Parser Program
programParser =
  Program
    <$> functionDefinitionParser
    <*> (string "main ()" *> spaces *> blockParse)
    <* spaces
    <* string "end"