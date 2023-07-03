module Program where

import Grammar
import Parsers
import Statement
import Text.Parsec
import Text.Parsec.String

functionsDefinitionParser :: Parser FunctionDefinitionList
functionsDefinitionParser = functionDefinitionParser `sepBy` (char '\n' *> spaces) >>= \functions ->
  return (analyzeFunctions functions)

analyzeFunctions :: [FunctionDefinition] -> FunctionDefinitionList
analyzeFunctions functions = 
  if all analyzeFunction functions 
    then FuncDefList functions 
  else FuncDefListError (ErrorType Type)

analyzeFunction :: FunctionDefinition -> Bool
analyzeFunction fc = case fc of
  (FuncDefinitionError _) -> False
  _ -> True

programParser :: Parser (Either Error Program)
programParser =
  try analyzeFunctionList
    <|> pure (Left (ErrorType Syntax))

analyzeFunctionList :: Parser (Either Error Program)
analyzeFunctionList = functionsDefinitionParser >>= \functions ->
  case functions of
    (FuncDefList _) -> return (Right (Program functions))
    (FuncDefListError err) -> return (Left err)

textReader :: IO ()
textReader = do
  fileText <- readFile "../resources/input.rav"
  print (parse programParser "../resources/input.rav" fileText)