module PrintStatementTemplate where

import Grammar
import ExpressionTemplate

printStatementTemplate :: String -> String
printStatementTemplate exp = "main :: IO()" ++ "\n" ++ "main = print" ++ "(" ++ exp ++ ")"

printStatementParserTransformer :: PrintStatement -> String
printStatementParserTransformer (PrintStatement exp) = expressionTransformer exp
