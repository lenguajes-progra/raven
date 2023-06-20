module Type where

import Data.Functor (($>))
import Error
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

variableDefinitionParser :: Parser VariableDefinition
variableDefinitionParser =
  try variableDefinitionCompleteParser
    <|> try variableDefinitionWithoutAssignmentParser
    <|> try variableDefinitionWithAssignmentParser

variableDefinitionCompleteParser :: Parser VariableDefinition
variableDefinitionCompleteParser =
  typeParser >>= \t ->
    spaces
      >> identifierParser
      >>= \identifier ->
        spaces
          >> (char '=' *> spaces *> (try (literalParserMatchesType t) <|> (Left <$> errorParser)))
          >>= \literal ->
              return (VariableDefinitionComplete t identifier literal)

literalParserMatchesType :: Type -> Parser (Either Error Literal)
literalParserMatchesType tp =
  literalParser >>= \literal ->
    if literalMatchesType tp literal
      then return (Right literal)
      else return (Left (ErrorType AssignType))

literalMatchesType :: Type -> Literal -> Bool
literalMatchesType tp literal = case (tp, literal) of
  (IntType, IntegerLiteral _) -> True
  (CharType, CharacterLiteral _) -> True
  (BooleanType, BooleanLiteral _) -> True
  (StringType, StringLiteral _) -> True
  _ -> False

variableDefinitionWithoutAssignmentParser :: Parser VariableDefinition
variableDefinitionWithoutAssignmentParser =
  VariableDefinitionWithoutAssignment
    <$> typeParser
    <* spaces
    <*> identifierParser

variableDefinitionWithAssignmentParser :: Parser VariableDefinition
variableDefinitionWithAssignmentParser =
  VariableDefinitionWithAssignment
    <$> identifierParser
    <* spaces
    <*> (char '=' *> spaces *> literalParser)

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
          >> (char '=' *> spaces *> (try (elementListParserMatchesType dataType) <|> (Left <$> errorParser)))
          >>= \errorOrElements ->
            return (ArrayDefinitionComplete dataType identifier errorOrElements)

elementListParserMatchesType :: Type -> Parser (Either Error ElementList)
elementListParserMatchesType tp =
  elementListParser >>= \elementList ->
    if elementsMatchType tp elementList
      then return (Right elementList)
      else return (Left (ErrorType AssignType))

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
