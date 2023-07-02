module DataTransformer where

data VariableType = TriNode String String String
                  | TwiceNodeWithAssignment String String
                  | TwiceNodeWithoutAssignment String String
                  deriving (Eq, Show)
