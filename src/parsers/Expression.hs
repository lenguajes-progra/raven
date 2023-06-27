module Expression where

import Data.Functor (($>))
import Function
import Grammar
  ( BitExpression (..),
    BitOperator (AndBit, LeftShift, OrBit, RightShift, XorBit),
    BooleanExpression (..),
    BooleanOperator (..),
    Expression (..),
    FunctionCall (..),
    LogicalExpression (..),
    LogicalOperator (And, Or),
    ParameterOption (..),
    ParametersCalled (..),
  )
import Literal
import Parsers
import Text.Parsec
import Text.Parsec.String
import Type

parseFromTo :: String -> a -> Parser a
parseFromTo s a = many space >> string s *> many space $> a

parseLiteralOrIdentifier :: Parser Expression
parseLiteralOrIdentifier =
  lexeme (Literal <$> try literalParser)
    <|> lexeme (Identifier <$> try identifierParser)

-- !INFO: Numeric Expression
parseEqual :: Parser BooleanOperator
parseEqual = parseFromTo "==" Equal

parseNotEqual :: Parser BooleanOperator
parseNotEqual = parseFromTo "!=" NotEqual

parseLessThan :: Parser BooleanOperator
parseLessThan = parseFromTo "<" LessThan

parseGreaterThan :: Parser BooleanOperator
parseGreaterThan = parseFromTo ">" GreaterThan

parseLessEqualThan :: Parser BooleanOperator
parseLessEqualThan = parseFromTo "<=" LessEqualThan

parseGreatEqualThan :: Parser BooleanOperator
parseGreatEqualThan = parseFromTo ">=" GreatEqualThan

parseNumericOperator :: Parser BooleanOperator
parseNumericOperator =
  try parseEqual
    <|> try parseNotEqual
    <|> try parseLessEqualThan
    <|> try parseGreatEqualThan
    <|> try parseLessThan
    <|> try parseGreaterThan

parseBooleanExpression :: Parser BooleanExpression
parseBooleanExpression = BooleanOp <$> try term <*> try parseNumericOperator <*> try parseExpression

-- !INFO: Bit Expression
parseAndBitOperator :: Parser BitOperator
parseAndBitOperator = parseFromTo "&" AndBit

parseOrBitOperator :: Parser BitOperator
parseOrBitOperator = parseFromTo "|" OrBit

parseXorOperator :: Parser BitOperator
parseXorOperator = parseFromTo "^" XorBit

parseLeftShiftOperator :: Parser BitOperator
parseLeftShiftOperator = parseFromTo "<<" LeftShift

parseRightShiftOperator :: Parser BitOperator
parseRightShiftOperator = parseFromTo ">>" RightShift

parseBitOperator :: Parser BitOperator
parseBitOperator =
  try parseAndBitOperator
    <|> try parseAndBitOperator
    <|> try parseOrBitOperator
    <|> try parseXorOperator
    <|> try parseLeftShiftOperator
    <|> try parseRightShiftOperator

parseBitNot :: Parser BitExpression
parseBitNot = parseFromTo "~" BitNot <*> term

parseBitExpression :: Parser BitExpression
parseBitExpression =
  BitOp <$> term <*> parseBitOperator <*> parseExpression
    <|> try parseBitNot

-- !INFO: Logical Expression
parseAndOperator :: Parser LogicalOperator
parseAndOperator = parseFromTo "&&" And

parseOrOperator :: Parser LogicalOperator
parseOrOperator = parseFromTo "||" Or

parseLogicalOperator :: Parser LogicalOperator
parseLogicalOperator =
  try parseAndOperator
    <|> try parseOrOperator

parseLogicalNot :: Parser LogicalExpression
parseLogicalNot = parseFromTo "!" LogicNot <*> term

parseLogicalExpression :: Parser LogicalExpression
parseLogicalExpression =
  LogicOp <$> term <*> parseLogicalOperator <*> parseExpression
    <|> parseLogicalNot

-- !INFO: Parse Expression

parseExpression :: Parser Expression
parseExpression =
  BooleanExpression <$> try parseBooleanExpression
    <|> LogicalExpression <$> try parseLogicalExpression
    <|> BitExpression <$> try parseBitExpression
    <|> FuncCall <$> try parseFunctionCall
    <|> term

term :: Parser Expression
term = try parseLiteralOrIdentifier <|> try parens

parens :: Parser Expression
parens = Parens <$> betweenParens parseExpression

-- !INFO: Function Parser
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
    <* lexeme spaces