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
    spaces
      >> identifierParser
      >>= \name ->
        spaces
          >> char '(' *> parametersParser <* spaces <* char ')' <* spaces
          >>= \parameters ->
            char '{' *> spaces *> blockParse <* spaces <* char '}' <* spaces >>= \body ->
              string "return" *> spaces *> try (expressParserMatchesType returnType parameters body) <* spaces >>= \exprType ->
                string "end"
                  >> return (FuncDefinition returnType name parameters body exprType)

expressParserMatchesType :: Type -> Parameters -> Block -> Parser (Either Error Expression)
expressParserMatchesType tp p bl =
  parseExpression >>= \expr ->
    if expressMatchesType tp p bl expr
      then return (Right expr)
      else return (Left (ErrorType TypeFunction))

expressMatchesType :: Type -> Parameters -> Block -> Expression -> Bool
expressMatchesType tp p b (Identifier ident) = identMatchesType tp p b ident
expressMatchesType tp _ _ express = case (tp, express) of
  (IntType, BitExpression _) -> True
  (BooleanType, BooleanExpression _) -> True
  (BooleanType, LogicalExpression _) -> True
  (FunctionType, FuncCall _) -> True
  (_, Literal lit) -> Type.literalMatchesType tp lit
  _ -> False

identMatchesType :: Type -> Parameters -> Block -> Identifier -> Bool
identMatchesType t p b ident =
  identMatchesParametersType t p ident || identMatchesBlockType t b ident

identMatchesParametersType :: Type -> Parameters -> Identifier -> Bool
identMatchesParametersType _ (Parameters []) _ = False
identMatchesParametersType t (Parameters ((tp, iden) : ps)) ident
  | iden == ident && t == tp = True
  | otherwise = identMatchesParametersType t (Parameters ps) ident

identMatchesBlockType :: Type -> Block -> Identifier -> Bool
identMatchesBlockType _ (Block []) _ = False
identMatchesBlockType t (Block (def : bl)) ident =
  case def of
    VariableDefinition (VariableDefinitionComplete tp iden _) -> iden == ident && t == tp
    VariableDefinition (VariableDefinitionWithoutAssignment tp iden) -> iden == ident && t == tp
    _ -> identMatchesBlockType t (Block bl) ident

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