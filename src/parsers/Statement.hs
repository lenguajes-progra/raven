module Statement where

import Grammar
import Type
import Expression
import Function
import Parsers
import PrintStatement
import Text.Parsec
import Text.Parsec.String(Parser)

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
              string "return" *> spaces *> try (expressParserMatchesType returnType name parameters body) <* spaces >>= \definition ->
                string "end"
                  >> return definition

expressParserMatchesType :: Type -> Identifier -> Parameters -> Block -> Parser FunctionDefinition
expressParserMatchesType tp ident p bl =
  parseExpression >>= \expr ->
    if expressMatchesType tp p bl expr then
      case bl of
        (Block _) -> pure (FuncDefinition tp ident p bl expr)
        (BlockError err) -> pure (FuncDefinitionError err)
      else return (FuncDefinitionError (ErrorType TypeFunction))

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
    VariableDefinition (VariableDefinitionComplete tp iden _) -> (iden == ident && t == tp) || identMatchesBlockType t (Block bl) ident
    VariableDefinition (VariableDefinitionWithoutAssignment tp iden) -> (iden == ident && t == tp) || identMatchesBlockType t (Block bl) ident
    _ -> identMatchesBlockType t (Block bl) ident

statementParse :: Parser Statement
statementParse =
  try (VariableDefinition <$> try variableDefinitionParser)
    <|> try (ArrayDefinition <$> try arrayDefinitionParser)
    <|> try (IfStat <$> try ifStatementParser)
    <|> try (ForStat <$> try forStatementParser)
    <|> try (PrintStat <$> try printStatementParser)
    <|> try (FuncCallStat <$> try parseFunctionCall)
    <|> try (End <$> try (char '\n'))

blockParse :: Parser Block
blockParse = statementParse `sepBy` lexeme (char ';') >>= \statements ->
  return (analyzeStatements statements)

analyzeStatements :: [Statement] -> Block
analyzeStatements statements = case all analyzeStatement statements of
  True -> Block statements
  False -> BlockError (ErrorType AssignType)

analyzeStatement :: Statement -> Bool
analyzeStatement st = case st of
  (VariableDefinition (VariableErrorDefinition _)) -> False
  (ArrayDefinition (ArrayErrorDefinition _)) -> False
  _ -> True

ifStatementParser :: Parser IfStatement
ifStatementParser =
  IfStatement
    <$> (string "if" *> spaces *> char '(' *> parseExpression <* char ')')
    <*> statementParseBetweenBrackets
    <*> (spaces *> string "else" *> statementParseBetweenBrackets)
    <* spaces
    <* string "end"

expressionParseBetweenBrackets :: Parser Expression
expressionParseBetweenBrackets = between (lexeme (char '{')) (lexeme (char '}')) (spaces *> parseExpression <* spaces)

statementParseBetweenBrackets :: Parser Statement
statementParseBetweenBrackets = between (lexeme (char '{')) (lexeme (char '}')) (spaces *> statementParse <* spaces)

forStatementParser :: Parser ForStatement
forStatementParser =
  ForStatement
    <$> (string "for" *> spaces *> char '(' *> parseLiteralOrIdentifier <* char ';')
    <*> parseExpression
    <* spaces
    <* char ')'
    <*>expressionParseBetweenBrackets
    <* spaces
    <* string "end"
