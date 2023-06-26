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
  typeParser >>= \returnType ->
    spaces >>
    identifierParser >>= \name ->
      spaces >>
      char '(' *> parametersParser <* spaces <* char ')' <* spaces >>= \parameters ->
        char '{' *> spaces *> blockParse <* spaces <* char '}' <* spaces >>= \body ->
          string "return" *> spaces *> try (expressParserMatchesType returnType) <* spaces >>= \exprType ->
            string "end" >>
            return (FuncDefinition returnType name parameters body exprType)


expressParserMatchesType :: Type -> Parser (Either Error Expression)
expressParserMatchesType tp =
  parseExpression >>= \expr ->
  if expressMatchesType tp expr
  then return (Right expr)
  else return (Left (ErrorType TypeFunction))


expressMatchesType :: Type -> Expression -> Bool
expressMatchesType tp express = case (tp, express) of
  (IntType, BitExpression _) -> True
  (BooleanType, NumericExpression _) -> True
  (BooleanType, LogicalExpression _) -> True
  (FunctionType, FuncCall _) -> True
  (_, Identifier _) -> True
  (_, Literal lit) -> Type.literalMatchesType tp lit
  _ -> False

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