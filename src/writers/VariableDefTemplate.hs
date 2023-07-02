module VariableDefTemplate where

import Grammar
import DataTransformer
import LiteralTypeTemplate
import ExpressionTemplate

variableDefinitionTemplate :: String -> String -> String -> String
variableDefinitionTemplate typ identifier expression = identifier ++ " :: " ++ typ ++ "\n" ++ identifier ++ " = " ++ expression

variableDefBlockTemplate :: String -> String -> String
variableDefBlockTemplate identifier expression = "\twhere " ++ identifier ++ " = " ++ expression

variableDefinitionTransformer :: VariableDefinition -> VariableType
variableDefinitionTransformer (VariableDefinitionComplete typ identifier expression) = TriNode (typeTransformer typ) (identifierTransformer identifier) (expressionTransformer expression)
variableDefinitionTransformer (VariableDefinitionWithoutAssignment typ identifier) = TwiceNode (typeTransformer typ) (identifierTransformer identifier)
variableDefinitionTransformer (VariableDefinitionWithAssignment identifier expression) = TwiceNode (identifierTransformer identifier) (expressionTransformer expression)
variableDefinitionTransformer _ = undefined


