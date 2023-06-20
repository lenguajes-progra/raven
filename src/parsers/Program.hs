module Program where

import Grammar
import Parsers
import Statement
import Text.Parsec
import Text.Parsec.String

functionsDefinitionParser :: Parser FunctionDefinitionList
functionsDefinitionParser = FuncDefList <$> functionDefinitionParser `sepBy` lexeme (string "\n")

programParser :: Parser Program
programParser =
  Program
  <$> functionsDefinitionParser
    <*> ( lexeme (string "main()")
            *> lexeme (char '`')
            *> blockParse
            <* lexeme (char '`')
        )
    <* string "end"