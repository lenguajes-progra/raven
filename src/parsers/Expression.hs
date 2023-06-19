module Expression where

import Data.Functor (void, ($>))
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
  lexeme (Literal <$> try literalParser)
    <|> lexeme (Identifier <$> try identifierParser)

-- !INFO: Numeric Expression
parseEqual :: Parser NumericOperator
parseEqual = parseFromTo "==" Equal

parseNotEqual :: Parser NumericOperator
parseNotEqual = parseFromTo "!=" NotEqual

parseLessThan :: Parser NumericOperator
parseLessThan = parseFromTo "<" LessThan

parseGreaterThan :: Parser NumericOperator
parseGreaterThan = parseFromTo ">" GreaterThan

parseLessEqualThan :: Parser NumericOperator
parseLessEqualThan = parseFromTo "<=" LessEqualThan

parseGreatEqualThan :: Parser NumericOperator
parseGreatEqualThan = parseFromTo ">=" GreatEqualThan

parseNumericOperator :: Parser NumericOperator
parseNumericOperator =
  try parseEqual
    <|> try parseNotEqual
    <|> try parseLessEqualThan
    <|> try parseGreatEqualThan
    <|> try parseLessThan
    <|> try parseGreaterThan

parseNumericExpression :: Parser NumericExpression
parseNumericExpression = NumericOp <$> try term <*> try parseNumericOperator <*> try term

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
  NumericExpression <$> try parseNumericExpression
    <|> LogicalExpression <$> try parseLogicalExpression
    <|> BitExpression <$> try parseBitExpression
    <|> term

term :: Parser Expression
term = try parseLiteralOrIdentifier <|> try parens

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: Char -> Parser ()
symbol c = void $ lexeme $ char c

betweenParens :: Parser a -> Parser a
betweenParens p = symbol '(' *> p <* symbol ')'

parens :: Parser Expression
parens = Parens <$> betweenParens parseExpression
