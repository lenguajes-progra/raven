module Program where

import Grammar
import Parsers
import Statement
import Text.Parsec
import Text.Parsec.String

functionsDefinitionParser :: Parser FunctionDefinitionList
functionsDefinitionParser = FuncDefList <$> functionDefinitionParser `sepBy` (char '\n' *> spaces)

programParser :: Parser (Either Error Program)
programParser =
  try (Right <$> (Program
    <$> functionsDefinitionParser 
    <*> ( lexeme (string "main()")
            *> lexeme (char '`')
            *>  blockParse
            <* lexeme (char '`')
        )
    <* string "end"))
    <|> (Left <$> pure (ErrorType Syntax))

textReader :: IO ()
textReader = do
  fileText <- readFile "../resources/input.rav"
  print (parse programParser "../resources/input.rav" fileText)