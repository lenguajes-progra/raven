module VariableDefTemplate where

import DataTransformer
import ExpressionTemplate
import Grammar
import LiteralTypeTemplate

variableDefinitionTemplate :: String -> String -> String -> String
variableDefinitionTemplate typ identifier expression = identifier ++ " :: " ++ typ ++ "\n" ++ identifier ++ " = " ++ expression ++ "\n"

variableBodyTemplate :: String -> String -> String
variableBodyTemplate identifier expression = identifier ++ " = " ++ expression

variableWithoutAssignment :: String -> String
variableWithoutAssignment identifier = "\twhere " ++ identifier ++ " = "

variableDefBlockTemplate :: String -> String -> String
variableDefBlockTemplate identifier expression = "\twhere " ++ identifier ++ " = " ++ expression

variableExpressionTemplate :: String -> String
variableExpressionTemplate expression = expression

variableDefinitionTransformer :: VariableDefinition -> [FunctionDefinition] -> VariableType
variableDefinitionTransformer (VariableDefinitionComplete typ identifier expression) fd = TriNode (typeTransformer typ) (identifierTransformer identifier) (expressionTransformer expression fd)
variableDefinitionTransformer (VariableDefinitionWithoutAssignment typ identifier) _ = TwiceNodeWithoutAssignment (typeTransformer typ) (identifierTransformer identifier)
variableDefinitionTransformer (VariableDefinitionWithAssignment identifier expression) fd = TwiceNodeWithAssignment (identifierTransformer identifier) (expressionTransformer expression fd)
variableDefinitionTransformer _ _ = undefined
