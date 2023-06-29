module Type where

import Data.Functor (($>))
import Grammar
import Literal
import Text.Parsec
import Text.Parsec.String

identifierParser :: Parser Identifier
identifierParser = Ident <$> ((:) <$> letter <*> many (letter <|> digit <|> char '_'))

elementParser :: Parser Element
elementParser =
  try (LiteralElement <$> literalParser)
    <|> try (IdentifierElement <$> identifierParser)

elementListParser :: Parser ElementList
elementListParser = Literals <$> (char '[' *> spaces *> literalParser `sepBy` (char ',' *> spaces) <* spaces <* char ']')

typeParser :: Parser Type
typeParser =
  try (string "int" $> IntType)
    <|> try (string "char" $> CharType)
    <|> try (string "boolean" $> BooleanType)
    <|> try (string "string" $> StringType)
    <|> try (string "function" $> FunctionType)

typeParserArray :: Parser Type
typeParserArray =
  try typeParser
    <|> try (ArrayType <$> (spaces *> char '[' >> spaces >> typeParser <* spaces <* char ']'))

-- Array definition
arrayDefinitionParser :: Parser ArrayDefinition
arrayDefinitionParser =
  try arrayDefinitionCompleteParser
    <|> try arrayDefinitionWithoutAssignmentParser
    <|> try arrayDefinitionWithAssignmentParser

arrayDefinitionCompleteParser :: Parser ArrayDefinition
arrayDefinitionCompleteParser =
  typeParserArray >>= \dataType ->
    spaces
      >> identifierParser
      >>= \identifier ->
        spaces
          >> (char '=' *> spaces *> elementListParserMatchesType dataType identifier)
          >>= \errorOrElements ->
            return errorOrElements

literalMatchesType :: Type -> Literal -> Bool
literalMatchesType tp literal = case (tp, literal) of
  (IntType, IntegerLiteral _) -> True
  (CharType, CharacterLiteral _) -> True
  (BooleanType, BooleanLiteral _) -> True
  (StringType, StringLiteral _) -> True
  _ -> False

elementListParserMatchesType :: Type -> Identifier -> Parser ArrayDefinition
elementListParserMatchesType tp ident =
  elementListParser >>= \elementList ->
    if elementsMatchType tp elementList
      then pure (ArrayDefinitionComplete tp ident elementList)
      else pure (ArrayErrorDefinition (ErrorType AssignType))

elementsMatchType :: Type -> ElementList -> Bool
elementsMatchType (ArrayType tp) (Literals literals) = all (literalMatchesType tp) literals

arrayDefinitionWithoutAssignmentParser :: Parser ArrayDefinition
arrayDefinitionWithoutAssignmentParser =
  ArrayDefinitionWithoutAssignment
    <$> typeParserArray
    <* spaces
    <*> identifierParser

arrayDefinitionWithAssignmentParser :: Parser ArrayDefinition
arrayDefinitionWithAssignmentParser =
  ArrayDefinitionWithAssignment
    <$> identifierParser
    <* spaces
    <*> (char '=' *> spaces *> elementListParser)
