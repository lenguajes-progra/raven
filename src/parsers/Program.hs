module Program where

import Grammar
import Parsers
import Statement
import Text.Parsec
import Text.Parsec.String

programParser :: Parser Program
programParser =
  Program
    <$> ( lexeme (string "main()")
            *> lexeme (char '`')
            *> blockParse
            <* lexeme (char '`')
        )
    <* string "end"