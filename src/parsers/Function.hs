module Function where

import Expression
import Grammar
import Text.Parsec
import Text.Parsec.String
import Type

parameterParser :: Parser (Type, Identifier)
parameterParser = (,) <$> typeParser <* spaces <*> identifierParser

parametersParser :: Parser Parameters
parametersParser = Parameters <$> parameterParser `sepBy` (char ',' *> spaces)

functionDefinitionParser :: Parser FunctionDefinition
functionDefinitionParser =
  FuncDefinition
    <$> typeParser <* spaces
    <*> identifierParser <* spaces
    <*> (char '(' *> parametersParser <* spaces <* char ')') <* spaces
    <*> string "block" <* spaces
    <*> parseExpression <* spaces
    <* string "end"
