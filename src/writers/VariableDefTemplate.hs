module VariableDefTemplate where

import Grammar
    ( VariableDefinition( .. ),
      Type(..),
      Identifier(..),
      Literal( .. ), FunctionDefinition (FuncDefinition) )
import Data.List

variableDefinitionTemplate :: String -> String -> String -> String
variableDefinitionTemplate typ identifier literal = identifier ++ " :: " ++ typ ++ "\n" ++ identifier ++ " = " ++ literal

variableDefinitionTransformer :: VariableDefinition -> (String, String, String)
variableDefinitionTransformer (VariableDefinitionComplete typ identifier literal) = (typeTransformer typ, identifierTransformer identifier, show literal)
variableDefinitionTransformer (VariableDefinitionWithoutAssignment typ identifier) = (typeTransformer typ, identifierTransformer identifier, "")
variableDefinitionTransformer (VariableDefinitionWithAssignment identifier literal) = (identifierTransformer identifier, show literal, "")

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
functionTypeTemplate (identifier, typ) parameters block = identifier ++ " :: " ++ intercalate " -> " parameters ++ " -> " ++ typ 

functionDefinitionTemplate :: String -> [String] -> String -> String
functionDefinitionTemplate identifier parameters expression = identifier ++ " " ++ intercalate " " parameters ++ " = " ++ expression

functionTransformer :: FunctionDefinition -> (String, [String], String)
functionTransformer (FuncDefinition typ identifier parameters block expression) = (identifierTransformer identifier, map show parameters, show expression)