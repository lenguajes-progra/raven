module Error where

data Error = ErrorType String

data ErrorType = Syntax
               | Type
               | FunctionType
               | AssignType