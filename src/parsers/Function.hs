module Function where

import Expression
import Grammar
import Literal
import Parsers
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
    <$> typeParser
    <* spaces
    <*> identifierParser
    <* spaces
    <*> (char '(' *> parametersParser <* spaces <* char ')')
    <* spaces
    <*> string "block"
    <* spaces
    <*> parseExpression
    <* spaces
    <* string "end"

parseParameterOption :: Parser ParameterOption
parseParameterOption =
  ParamLiteral <$> lexeme literalParser
    <|> ParamIdentifier <$> lexeme identifierParser

parseParametersCalled :: Parser ParametersCalled
parseParametersCalled =
  ParametersCalled <$> sepBy parseParameterOption (lexeme (char ','))

parseFunctionCall :: Parser FunctionCall
parseFunctionCall =
  FunctionCall
    <$> lexeme identifierParser
    <*> between (lexeme (char '(')) (lexeme (char ')')) (lexeme parseParametersCalled)
    <* lexeme (char ';')