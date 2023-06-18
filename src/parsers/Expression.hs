module Expression where

import Data.Functor (($>))
import Grammar
import Literal
import Parsers
import Text.Parsec
import Text.Parsec.String
import Type 

parseFromTo :: String -> a -> Parser a
parseFromTo s a = many space >> string s *> many space $> a

parseLiteralOrIdentifier :: Parser Expression
parseLiteralOrIdentifier =
  Identifier <$> try identifierParser
    <|> Literal <$> try literalParser

-- INFO: Numeric Expression

parseEqual :: Parser NumericOperator
parseEqual = parseFromTo "==" Equal

parseNotEqual :: Parser NumericOperator
parseNotEqual = parseFromTo "!=" NotEqual

parseGreaterThan :: Parser NumericOperator
parseGreaterThan = parseFromTo ">" GreaterThan

parseLessThan :: Parser NumericOperator
parseLessThan = parseFromTo "<" LessThan

parseGreaterEqualThan :: Parser NumericOperator
parseGreaterEqualThan = parseFromTo ">=" GreatEqualThan

parseLessEqualThan :: Parser NumericOperator
parseLessEqualThan = parseFromTo "<=" LessEqualThan

parseNumericOperator :: Parser NumericOperator
parseNumericOperator = try parseEqual <|> try parseNotEqual <|> try parseGreaterEqualThan <|> try parseLessEqualThan <|> try parseGreaterThan <|> try parseLessThan

parseNumericExpression :: Parser NumericExpression
parseNumericExpression = NumExpr <$> try parseLiteralOrIdentifier <*> try parseNumericOperator <*> try parseLiteralOrIdentifier

-- INFO: Bit Expression

parseAndBit :: Parser BitOperator
parseAndBit = parseFromTo "&" AndBit

parseOrBit :: Parser BitOperator
parseOrBit = parseFromTo "|" OrBit

parseXorBit :: Parser BitOperator
parseXorBit = parseFromTo "^" XorBit

parseRightShiftBit :: Parser BitOperator
parseRightShiftBit = parseFromTo ">>" RightShift

parseLeftShiftBit :: Parser BitOperator
parseLeftShiftBit = parseFromTo "<<" LeftShift

parseNotBit :: Parser BitOperator
parseNotBit = parseFromTo "~" NotBit

parseBitOperator :: Parser BitOperator
parseBitOperator = try parseAndBit <|> try parseOrBit <|> try parseXorBit <|> try parseRightShiftBit <|> try parseLeftShiftBit <|> parseNotBit

parseBitExpression :: Parser BitExpression
parseBitExpression =
  BitExpr <$> try parseLiteralOrIdentifier <*> try parseBitOperator <*> try parseLiteralOrIdentifier
    <|> BitNot <$> parseBitOperator <*> try parseLiteralOrIdentifier

-- INFO: Logical Expression

parseAnd :: Parser LogicalOperator
parseAnd = parseFromTo "&&" And

parseOr :: Parser LogicalOperator
parseOr = parseFromTo "||" Or

parseNot :: Parser LogicalOperator
parseNot = parseFromTo "!" Not

parseLogicalOperator :: Parser LogicalOperator
parseLogicalOperator = try parseAnd <|> try parseOr <|> try parseNot

parseLogicalExpression :: Parser LogicalExpression
parseLogicalExpression =
  LogExpr <$> try parseLiteralOrIdentifier <*> try parseLogicalOperator <*> try parseLiteralOrIdentifier
    <|> LogicNot <$> try parseLogicalOperator <*> try parseLiteralOrIdentifier

parseExpression :: Parser Expression
parseExpression =
  NumericExpression <$> try parseNumericExpression
    <|> LogicalExpression <$> try parseLogicalExpression
    <|> BitExpression <$> try parseBitExpression
    <|> try parseLiteralOrIdentifier
