module IfTemplate where
import Grammar
import ExpressionTemplate

ifTemplate :: Expression -> Expression -> Expression -> String
ifTemplate cond exp1 exp2 = "if" ++ " " ++ expressionTransformer cond ++ " " ++ "then" ++ " " ++ expressionTransformer exp1 ++ " " ++ "else" ++ " " ++ expressionTransformer exp2