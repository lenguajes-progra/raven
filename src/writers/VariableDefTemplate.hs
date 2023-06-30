module VariableDefTemplate where

import Grammar
import Data.List
import LiteralTemplate
import PrintStatementTemplate (expressionTransformer)

variableDefinitionTemplate :: String -> String -> String -> String
variableDefinitionTemplate typ identifier literal = identifier ++ " :: " ++ typ ++ "\n" ++ identifier ++ " = " ++ literal

variableDefinitionTransformer :: VariableDefinition -> VariableType
variableDefinitionTransformer (VariableDefinitionComplete typ identifier expression) = TriNode (typeTransformer typ) (identifierTransformer identifier) (expressionTransformer expression)
variableDefinitionTransformer (VariableDefinitionWithoutAssignment typ identifier) = TwiceNode (typeTransformer typ) (identifierTransformer identifier)
variableDefinitionTransformer (VariableDefinitionWithAssignment identifier expression) = TwiceNode (identifierTransformer identifier) (expressionTransformer expression)

data VariableType = TriNode String String String
                  | TwiceNode String String
                  deriving (Show)

typeTransformer :: Type -> String
typeTransformer word = case word of
    IntType -> "Int"
    CharType -> "Char"
    BooleanType -> "Bool"
    StringType -> "String"
    FunctionType -> "(" ++ "Function" ++ ")"
    (ArrayType a) -> "[" ++ typeTransformer a ++ "]"

identifierTransformer :: Identifier -> String
identifierTransformer (Ident a) = takeWhile (/= '\"') (tail (show a))

functionTypeTemplate :: (String, String) -> [String] -> [String] -> String
functionTypeTemplate (typ, identifier) parameters block = identifier ++ " :: " ++ intercalate " -> " parameters ++ " -> " ++ typ 

functionDefinitionTemplate :: String -> [String] -> String -> String
functionDefinitionTemplate identifier parameters expression = identifier ++ " " ++ intercalate " " parameters ++ " = " ++ expression

functionTransformer :: FunctionDefinition -> (String, [String], String)
functionTransformer (FuncDefinition typ identifier parameters block expression) = (identifierTransformer identifier, parametersTransformer parameters, show expression)

parametersTransformer :: Parameters -> [String]
parametersTransformer (Parameters []) = []
parametersTransformer (Parameters ((_, identifierParameter):ps)) = identifierTransformer identifierParameter:parametersTransformer (Parameters ps)



