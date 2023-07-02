module ArrayDefinitionTemplate where

import Grammar
import DataTransformer
import LiteralTypeTemplate

arrayDefinitionTemplate :: String -> String -> String -> String
arrayDefinitionTemplate arrType name literal = name ++ "::" ++ arrType ++ " \n " ++ name ++ "=" ++ literal

arrayBodyTemplate :: String -> String -> String
arrayBodyTemplate identifier literal = identifier ++ " = " ++ literal

arrayWithoutAssignment :: String -> String
arrayWithoutAssignment identifier = "\twhere " ++ identifier ++ " = "

elementListTransformer' :: ElementList -> String
elementListTransformer' (Literals []) = ""
elementListTransformer' (Literals (arr : as)) =
  if null as
    then literalTransformer arr
    else literalTransformer arr ++ ", " ++ elementListTransformer' (Literals as)

elementListTransformer :: ElementList -> String
elementListTransformer exp = "[" ++ elementListTransformer' exp ++ "]"

arrayDefinitionTransformer :: ArrayDefinition -> VariableType
arrayDefinitionTransformer (ArrayDefinitionComplete arrType identifier elementList) = TriNode (typeTransformer arrType) (identifierTransformer identifier) (elementListTransformer elementList)
arrayDefinitionTransformer (ArrayDefinitionWithoutAssignment arrType identifier) = TwiceNodeWithoutAssignment (typeTransformer arrType) (identifierTransformer identifier)
arrayDefinitionTransformer (ArrayDefinitionWithAssignment identifier elementList) = TwiceNodeWithAssignment (identifierTransformer identifier) (elementListTransformer elementList)
arrayDefinitionTransformer _ = undefined
