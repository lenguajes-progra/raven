module VariableDefTemplate where

import Grammar
    ( VariableDefinition( .. ),
      Type(..),
      Identifier(..),
      Literal( .. ) )

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

