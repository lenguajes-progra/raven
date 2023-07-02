module ArrayDefinitionTemplate where

import Grammar
import DataTransformer
import LiteralTypeTemplate

arrayTemplate :: String -> String -> String -> String
arrayTemplate arrType name exp = name ++ "::" ++ arrType ++ " \n " ++ name ++ "=" ++ exp

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
arrayDefinitionTransformer (ArrayDefinitionWithoutAssignment arrType identifier) = TwiceNode (typeTransformer arrType) (identifierTransformer identifier)
arrayDefinitionTransformer (ArrayDefinitionWithAssignment identifier elementList) = TwiceNode (identifierTransformer identifier) (elementListTransformer elementList)
arrayDefinitionTransformer _ = undefined
