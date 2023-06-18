module Expression where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Literal (Literal (..), literalParser)
import Parsers
import Text.Parsec (many, space, string, try)
import Text.Parsec.String
import Type (Identifier (..), identifierParser)

data Expression
  = Literal Literal
  | Identifier Identifier
  | NumericExpression NumericExpression
  | LogicalExpression LogicalExpression
  | BitExpression BitExpression
  deriving (Show)

data LogicalExpression
  = LogicNot LogicalOperator Expression
  | LogExpr Expression LogicalOperator Expression
  deriving (Show)

data BitExpression
  = BitNot BitOperator Expression
  | BitExpr Expression BitOperator Expression
  deriving (Show)

data NumericExpression
  = NumExpr Expression NumericOperator Expression
  deriving (Show)

data LogicalOperator = And | Or | Not deriving (Show)

data BitOperator = AndBit | OrBit | XorBit | RightShift | LeftShift | NotBit deriving (Show)

data NumericOperator = Equal | NotEqual | LessThan | GreaterThan | LessEqualThan | GreatEqualThan deriving (Show)

-- INFO: Numeric Expression

parseEqual :: Parser NumericOperator
parseEqual = many space >> string "==" *> many space $> Equal

parseNotEqual :: Parser NumericOperator
parseNotEqual = many space >> string "!=" *> many space $> NotEqual

parseGreaterThan :: Parser NumericOperator
parseGreaterThan = many space >> string ">" *> many space $> GreaterThan

parseLessThan :: Parser NumericOperator
parseLessThan = many space >> string "<" *> many space $> LessThan

parseGreaterEqualThan :: Parser NumericOperator
parseGreaterEqualThan = many space >> string ">=" *> many space $> GreatEqualThan

parseLessEqualThan :: Parser NumericOperator
parseLessEqualThan = many space >> string "<=" *> many space $> LessEqualThan

parseNumericOperator :: Parser NumericOperator
parseNumericOperator = try parseEqual <|> try parseNotEqual <|> try parseGreaterEqualThan <|> try parseLessEqualThan <|> try parseGreaterThan <|> try parseLessThan

parseLiteralOrIdentifier :: Parser Expression
parseLiteralOrIdentifier = Identifier <$> try identifierParser <|> Literal <$> try literalParser

parseNumericExpression :: Parser NumericExpression
parseNumericExpression = NumExpr <$> parseLiteralOrIdentifier <*> parseNumericOperator <*> parseLiteralOrIdentifier

-- INFO: Bit Expression

parseAndBit :: Parser BitOperator
parseAndBit = many space >> string "&" *> many space $> AndBit

parseOrBit :: Parser BitOperator
parseOrBit = many space >> string "|" *> many space $> OrBit

parseXorBit :: Parser BitOperator
parseXorBit = many space >> string "^" *> many space $> XorBit

parseRightShiftBit :: Parser BitOperator
parseRightShiftBit = many space >> string ">>" *> many space $> RightShift

parseLeftShiftBit :: Parser BitOperator
parseLeftShiftBit = many space >> string "<<" *> many space $> LeftShift

parseNotBit :: Parser BitOperator
parseNotBit = many space >> string "~" *> many space $> NotBit

parseBitOperator :: Parser BitOperator
parseBitOperator = try parseAndBit <|> try parseOrBit <|> try parseXorBit <|> try parseRightShiftBit <|> try parseLeftShiftBit <|> parseNotBit

parseBitExpression :: Parser BitExpression
parseBitExpression =
  BitExpr <$> parseLiteralOrIdentifier <*> parseBitOperator <*> parseLiteralOrIdentifier
    <|> BitNot <$> parseBitOperator <*> parseLiteralOrIdentifier

-- INFO: Logical Expression

parseAnd :: Parser LogicalOperator
parseAnd = many space >> string "&&" *> many space $> And

parseOr :: Parser LogicalOperator
parseOr = many space >> string "||" *> many space $> Or

parseNot :: Parser LogicalOperator
parseNot = many space >> string "!" *> many space $> Not

parseLogicalOperator :: Parser LogicalOperator
parseLogicalOperator = try parseAnd <|> try parseOr <|> try parseNot

parseLogicalExpression :: Parser LogicalExpression
parseLogicalExpression =
  LogExpr <$> parseLiteralOrIdentifier <*> parseLogicalOperator <*> parseLiteralOrIdentifier
    <|> LogicNot <$> parseLogicalOperator <*> parseLiteralOrIdentifier

parseExpression :: Parser Expression
parseExpression =
  NumericExpression <$> try parseNumericExpression
    <|> LogicalExpression <$> try parseLogicalExpression
    <|> BitExpression <$> try parseBitExpression
    <|> try parseLiteralOrIdentifier
