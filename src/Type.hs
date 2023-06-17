module Type where

import Text.Parsec
import Text.Parsec.String
import Literal

data Identifier = Identifier String
                deriving (Show)

data Element = LiteralElement Literal
             | IdentifierElement Identifier
             | ArrayElement [Element]
             deriving (Show)

data Type = IntType
          | CharType
          | BooleanType
          | StringType
          | FunctionType
          | ArrayType Type
          deriving (Show)

data VariableDefinition = VariableDefinitionComplete Type Identifier (Either String Literal)
                         | VariableDefinitionWithoutAssignment Type Identifier
                         | VariableDefinitionWithAssignment Identifier Literal
                         deriving (Show)

data ArrayDefinition = ArrayDefinitionType Type Identifier (Maybe Element)
                     | ArrayAssignment Identifier Element
                     deriving (Show)

identifierParser :: Parser Identifier
identifierParser = Identifier <$> ((:) <$> letter <*> many (letter <|> digit <|> char '_'))

elementParser :: Parser Element
elementParser = LiteralElement <$> literalParser
             <|>IdentifierElement <$> identifierParser

elementListParser :: Parser Element
elementListParser = ArrayElement <$> arrayParser

arrayParser :: Parser [Element]
arrayParser = between (char '[' >> spaces) (spaces >> char ']') (elementParser `sepBy` (char ',' >> spaces))

typeParser :: Parser Type
typeParser = string "int" *> pure IntType
         <|> string "char" *> pure CharType
         <|> string "boolean" *> pure BooleanType
         <|> string "string" *> pure StringType
         <|> string "function" *> pure FunctionType
         <|> ArrayType <$> (spaces *> char '[' >> spaces >> typeParser <* spaces <* char ']')

variableDefinitionParser :: Parser VariableDefinition
variableDefinitionParser = variableDefinitionCompleteParser
                        <|> variableDefinitionWithoutAssignmentParser
                        <|> variableDefinitionWithAssignmentParser

variableDefinitionCompleteParser :: Parser VariableDefinition
variableDefinitionCompleteParser = VariableDefinitionComplete
        <$> typeParser <* spaces
        <*> identifierParser <* spaces
        <*> (char '=' *> spaces *> ((Left <$> string "error") <|> (Right <$> literalParser)))
        <* char ';'

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
arrayDefinitionParser = choice [arrayDefinitionTypeParser,
                                   arrayAssignmentParser]

arrayDefinitionTypeParser :: Parser ArrayDefinition
arrayDefinitionTypeParser = do
  varType <- typeParser
  spaces
  varIdentifier <- identifierParser
  spaces
  assignment <- optionMaybe (char '=' >> spaces >> elementListParser)
  _ <- char ';'
  return (ArrayDefinitionType varType varIdentifier assignment)


arrayAssignmentParser :: Parser ArrayDefinition
arrayAssignmentParser = do
  varIdentifier <- identifierParser
  spaces
  assignment <- char '=' >> spaces >> elementParser
  _ <- char ';'
  return (ArrayAssignment varIdentifier assignment)
