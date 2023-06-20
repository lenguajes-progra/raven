module Statement where

import Expression
import Function
import Grammar
import Parsers
import PrintStatement
import Text.Parsec
import Text.Parsec.String
import Type

functionDefinitionParser :: Parser FunctionDefinition
functionDefinitionParser =
  FuncDefinition
    <$> typeParser
    <* spaces
    <*> identifierParser
    <* spaces
    <*> (char '(' *> parametersParser <* spaces <* char ')')
    <* spaces
    <*> blockParse
    <* spaces
    <*> parseExpression
    <* spaces
    <* string "end"

statementParse :: Parser Statement
statementParse =
  try (VariableDefinition <$> variableDefinitionParser)
    <|> try (Expression <$> parseExpression)
    <|> try (ArrayDefinition <$> try arrayDefinitionParser)
    <|> try (IfStat <$> try ifStatementParser)
    <|> try (PrintStat <$> try printStatementParser)
    <|> try (FuncCallStat <$> try parseFunctionCall)
    <|> try (End <$> try (char '\n'))

blockParse :: Parser Block
blockParse = Block <$> statementParse `sepBy` (char ';' *> spaces)

ifStatementParser :: Parser IfStatement
ifStatementParser =
  IfStatement
    <$> (string "if" *> spaces *> char '(' *> parseExpression <* char ')')
    <*> (spaces *> blockParse)
    <*> (spaces *> string "else" *> spaces *> blockParse)
    <* spaces
    <* string "end"