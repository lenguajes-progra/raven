module Statement where

import Expression
import Grammar
import If
import Parsers
import PrintStatement
import Text.Parsec
import Text.Parsec.String
import Type

statementParse :: Parser Statement
statementParse =
  try (Expression <$> parseExpression <* lexeme (char ';'))
    <|> try (VariableDefinition <$> variableDefinitionParser)
    <|> try (ArrayDefinition <$> try arrayDefinitionParser)
    <|> try (IfStat <$> try ifStatementParser)
    <|> try (PrintStat <$> try printStatementParser)
    <|> try (FuncCallStat <$> try parseFunctionCall)