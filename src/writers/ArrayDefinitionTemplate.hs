module ArrayDefinitionTemplate where

import Grammar
import DataTransformer
import LiteralTypeTemplate

arrayTemplate :: String -> String -> String -> String
arrayTemplate arrType name exp = name ++ "::" ++ "[" ++ arrType ++ "]" ++ " \n " ++ name ++ "=" ++ exp

elementListTransformer' :: ElementList -> String
elementListTransformer' (Literals []) = ""
elementListTransformer' (Literals (arr : as)) =
  if length as == 0
    then literalTransformer arr
    else literalTransformer arr ++ ", " ++ elementListTransformer' (Literals as)

elementListTransformer :: ElementList -> String
elementListTransformer exp = "[" ++ elementListTransformer' exp ++ "]"

arrayDefinitionTransformer :: ArrayDefinition -> VariableType
arrayDefinitionTransformer (ArrayDefinitionComplete arrType identifier elementList) = TriNode (typeTransformer arrType) (identifierTransformer identifier) (elementListTransformer elementList)
arrayDefinitionTransformer (ArrayDefinitionWithoutAssignment arrType identifier) = TwiceNode (typeTransformer arrType) (identifierTransformer identifier)
arrayDefinitionTransformer (ArrayDefinitionWithAssignment identifier elementList) = TwiceNode (identifierTransformer identifier) (elementListTransformer elementList)

