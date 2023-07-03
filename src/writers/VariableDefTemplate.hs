module VariableDefTemplate where

import Grammar
import DataTransformer
import LiteralTypeTemplate
import ExpressionTemplate

variableDefinitionTemplate :: String -> String -> String -> String
variableDefinitionTemplate typ identifier expression = identifier ++ " :: " ++ typ ++ "\n" ++ identifier ++ " = " ++ expression

variableBodyTemplate :: String -> String -> String
variableBodyTemplate identifier expression = identifier ++ " = " ++ expression

variableWithoutAssignment :: String -> String
variableWithoutAssignment identifier = "\twhere " ++ identifier ++ " = "

variableDefBlockTemplate :: String -> String -> String
variableDefBlockTemplate identifier expression = "\twhere " ++ identifier ++ " = " ++ expression

variableExpressionTemplate :: String -> String
variableExpressionTemplate expression = expression

variableDefinitionTransformer :: VariableDefinition -> VariableType
variableDefinitionTransformer (VariableDefinitionComplete typ identifier expression) = TriNode (typeTransformer typ) (identifierTransformer identifier) (expressionTransformer expression)
variableDefinitionTransformer (VariableDefinitionWithoutAssignment typ identifier) = TwiceNodeWithoutAssignment (typeTransformer typ) (identifierTransformer identifier)
variableDefinitionTransformer (VariableDefinitionWithAssignment identifier expression) = TwiceNodeWithAssignment (identifierTransformer identifier) (expressionTransformer expression)
variableDefinitionTransformer _ = undefined


