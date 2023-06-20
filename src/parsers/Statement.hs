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
    <*> (lexeme (char '{') *> spaces *> blockParse <* spaces <* lexeme (char '}'))
    <*> (string "return" *> spaces *> parseExpression <* spaces)
    <* string "end"

statementParse :: Parser Statement
statementParse =
  try (VariableDefinition <$> try variableDefinitionParser)
    <|> try (ArrayDefinition <$> try arrayDefinitionParser)
    <|> try (IfStat <$> try ifStatementParser)
    <|> try (LoopStat <$> try loopStatementParser)
    <|> try (PrintStat <$> try printStatementParser)
    <|> try (FuncCallStat <$> try parseFunctionCall)
    <|> try (Expression <$> try parseExpression)
    <|> try (End <$> try (char '\n'))

blockParse :: Parser Block
blockParse = Block <$> statementParse `sepBy` lexeme (char ';')

ifStatementParser :: Parser IfStatement
ifStatementParser =
  IfStatement
    <$> (string "if" *> spaces *> char '(' *> parseExpression <* char ')')
    <*> blockParseBetweenBrackets
    <*> (spaces *> string "else" *> blockParseBetweenBrackets)
    <* spaces
    <* string "end"

blockParseBetweenBrackets :: Parser Block
blockParseBetweenBrackets = between (lexeme (char '{')) (lexeme (char '}')) (spaces *> blockParse <* spaces)

loopStatementParser :: Parser LoopStatement
loopStatementParser =
  LoopStatement
    <$> (string "while" *> spaces *> char '(' *> parseExpression <* char ')')
    <*> blockParseBetweenBrackets
    <* spaces
    <* string "end"