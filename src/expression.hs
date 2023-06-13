module Expression where

data Expression = Integer deriving (Eq, Show, Ord)

data LogicalExpression
  = Not Expression
  | And Expression Expression
  | Or Expression Expression
  deriving (Eq, Show)

data BitExpression
  = NotBit Expression
  | AndBit Expression
  | OrBit Expression
  | XorBit Expression
  | RightShift Expression
  | LeftShift Expression
  deriving (Show, Eq)

data NumericExpression
  = Equal Expression Expression
  | NotEqual Expression Expression
  | LessThan Expression Expression
  | GreaterThan Expression Expression
  | LessEqualThan Expression Expression
  | GreatEqualThan Expression Expression
  deriving (Show, Eq, Ord)
