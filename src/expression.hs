module Expression where

import Control.Applicative ((<|>))
import Data.Functor (($>), (<&>))
import Literal (Literal (..), literalParser)
import Parsers
import Text.Parsec (char, choice, many, space, spaces, string, try)
import Text.Parsec.String
import Type (Identifier (..), identifierParser)

data Expression = Literal Literal | Identifier Identifier | NumericExpression NumericExpression deriving (Show)

data LogicalExpression
  = Not Expression
  | LogicalOperator Expression Expression
  deriving (Show)

data BitExpression
  = NotBit Expression
  | BitOperator Expression Expression
  deriving (Show)

data NumericExpression
  = NumericOp Expression NumericOperator Expression
  deriving (Show)

data LogicalOperator = And | Or deriving (Show)

data BitOperator = AndBit | OrBit | XorBit | RightShift | LeftShift deriving (Show)

data NumericOperator = Equal | NotEqual | LessThan | GreaterThan | LessEqualThan | GreatEqualThan deriving (Show)

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
-- parseNumericExpression = parseLiteralOrIdentifier >>= (\x -> parseNumericOperator >>= (\y -> parseLiteralOrIdentifier >>= (\z -> return $ NumericOp x y z)))
-- parseNumericExpression = parseLiteralOrIdentifier >>= (\x -> parseNumericOperator >>= (\y -> parseLiteralOrIdentifier <&> NumericOp x y))
parseNumericExpression = NumericOp <$> parseLiteralOrIdentifier <*> parseNumericOperator <*> parseLiteralOrIdentifier

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

parseBitOperator :: Parser BitOperator
parseBitOperator = try parseAndBit <|> try parseOrBit <|> try parseXorBit <|> try parseRightShiftBit <|> try parseLeftShiftBit

example = Ident "hello"

example2 = Identifier (Ident "hello")
