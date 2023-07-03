module PrintStatementTemplate where

import Grammar
import ExpressionTemplate

printStatementTemplate :: String -> String
printStatementTemplate exp = "main :: IO()" ++ "\n" ++ "main = print" ++ "(" ++ exp ++ ")"

printStatementTransformer :: PrintStatement -> String
printStatementTransformer (PrintStatement exp) = printStatementTemplate (expressionTransformer exp) ++ "\n"
