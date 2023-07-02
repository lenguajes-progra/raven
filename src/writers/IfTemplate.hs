module IfTemplate where

import ExpressionTemplate
import Grammar

ifTemplate :: Expression -> Expression -> Expression -> String
ifTemplate cond exp1 exp2 = "if" ++ " " ++ expressionTransformer cond ++ " " ++ "then" ++ " " ++ expressionTransformer exp1 ++ " " ++ "else" ++ " " ++ expressionTransformer exp2

ifTransformer :: IfStatement -> String
ifTransformer (IfStatement expr1 expr2 expr3) = ifTemplate expr1 expr2 expr3
