module PrintStatementTemplate where

import ExpressionTemplate
import Grammar

printStatementTemplate :: String -> String
printStatementTemplate exp = "main :: IO()" ++ "\n" ++ "main = print" ++ "(" ++ exp ++ ")"

printStatementTransformer :: PrintStatement -> [FunctionDefinition] -> String
printStatementTransformer (PrintStatement exp) fd = printStatementTemplate (expressionTransformer exp fd) ++ "\n"
