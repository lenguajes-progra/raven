module PrintStatementTemplate where

import ExpressionTemplate
import Grammar

printStatementTemplate :: String -> String
printStatementTemplate exp = "main :: IO()" ++ "\n" ++ "main = print" ++ "(" ++ exp ++ ")"

printStatementTransformer :: PrintStatement -> String
printStatementTransformer (PrintStatement exp) = printStatementTemplate (expressionTransformer exp) ++ "\n"
