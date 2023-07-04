module LiteralTypeTemplate where

import Grammar

literalTransformer :: Literal -> String
literalTransformer lit = case lit of
  (IntegerLiteral il) -> show il
  (BooleanLiteral bl) -> show bl
  (StringLiteral sl) -> show sl
  (CharacterLiteral cl) -> show cl

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
