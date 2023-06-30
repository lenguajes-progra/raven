module DataTransformer where

data VariableType = TriNode String String String
                  | TwiceNode String String
                  deriving (Eq, Show)
