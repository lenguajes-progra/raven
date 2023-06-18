module Type (Identifier (..), identifierParser) where

import Text.Parsec
import Text.Parsec.String
import Literal
import Error ( errorParser, ErrorType(Type), Error(..) )

data Identifier = Ident String
  deriving (Show)

data Element = LiteralElement Literal
             | IdentifierElement Identifier
             deriving (Show)

data ElementList = Literals [Literal]
                deriving (Show)

data Type = IntType
          | CharType
          | BooleanType
          | StringType
          | FunctionType
          | ArrayType Type
          deriving (Show)

data VariableDefinition = VariableDefinitionComplete Type Identifier (Either Error Literal)
                         | VariableDefinitionWithoutAssignment Type Identifier
                         | VariableDefinitionWithAssignment Identifier Literal
                         deriving (Show)

data ArrayDefinition = ArrayDefinitionComplete Type Identifier (Either Error ElementList)
                     | ArrayDefinitionWithoutAssignment Type Identifier
                     | ArrayDefinitionWithAssignment Identifier ElementList
                     deriving (Show)

identifierParser :: Parser Identifier
identifierParser = Ident <$> ((:) <$> letter <*> many (letter <|> digit <|> char '_'))

elementParser :: Parser Element
elementParser = try (LiteralElement <$> literalParser)
             <|>try (IdentifierElement <$> identifierParser)

elementListParser :: Parser ElementList
elementListParser = Literals <$> (char '[' *> spaces *> literalParser `sepBy` (char ',' *> spaces) <* spaces <* char ']')

typeParser :: Parser Type
typeParser = try (string "int" *> pure IntType)
         <|> try (string "char" *> pure CharType)
         <|> try (string "boolean" *> pure BooleanType)
         <|> try (string "string" *> pure StringType)
         <|> try (string "function" *> pure FunctionType)
         <|> try (ArrayType <$> (spaces *> char '[' >> spaces >> typeParser <* spaces <* char ']'))

variableDefinitionParser :: Parser VariableDefinition
variableDefinitionParser = try variableDefinitionCompleteParser
                        <|> try variableDefinitionWithoutAssignmentParser
                        <|> try variableDefinitionWithAssignmentParser

variableDefinitionCompleteParser :: Parser VariableDefinition
variableDefinitionCompleteParser = VariableDefinitionComplete
        <$> typeParser <* spaces
        <*> identifierParser <* spaces
        <*> (char '=' *> spaces *> ((Left <$> errorParser) <|> (Right <$> literalParser)))
        <* char ';'

literalParserMatchesType :: Type -> Parser (Either Error Literal)
literalParserMatchesType tp =
    literalParser >>= \literal ->
    if literalMatchesType tp literal
        then return (Right literal)
        else return (Left (ErrorType Type))


literalMatchesType :: Type -> Literal -> Bool
literalMatchesType tp literal = case (tp, literal) of
    (IntType, IntegerLiteral _) -> True
    (CharType, CharacterLiteral _) -> True
    (BooleanType, BooleanLiteral _) -> True
    (StringType, StringLiteral _) -> True
    _ -> False

variableDefinitionWithoutAssignmentParser :: Parser VariableDefinition
variableDefinitionWithoutAssignmentParser = VariableDefinitionWithoutAssignment
              <$> typeParser <* spaces
              <*> identifierParser <* char ';'

variableDefinitionWithAssignmentParser :: Parser VariableDefinition
variableDefinitionWithAssignmentParser = VariableDefinitionWithAssignment
        <$> identifierParser <* spaces
        <*> (char '=' *> spaces *> literalParser)
        <* char ';'

arrayDefinitionParser :: Parser ArrayDefinition
arrayDefinitionParser = try arrayDefinitionCompleteParser
                    <|> try arrayDefinitionWithoutAssignmentParser
                    <|> try arrayDefinitionWithAssignmentParser

arrayDefinitionCompleteParser :: Parser ArrayDefinition
arrayDefinitionCompleteParser = ArrayDefinitionComplete
        <$> typeParser <* spaces
        <*> identifierParser <* spaces
        <*> (char '=' *> spaces *> (try (Left <$> errorParser) <|> (Right <$> elementListParser)))

arrayDefinitionWithoutAssignmentParser :: Parser ArrayDefinition
arrayDefinitionWithoutAssignmentParser = ArrayDefinitionWithoutAssignment
              <$> typeParser <* spaces
              <*> identifierParser <* char ';'

arrayDefinitionWithAssignmentParser :: Parser ArrayDefinition
arrayDefinitionWithAssignmentParser = ArrayDefinitionWithAssignment
        <$> identifierParser <* spaces
        <*> (char '=' *> spaces *> elementListParser)
        <* char ';'
