module Type where

import Text.Parsec
import Text.Parsec.String (Parser)
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

data VariableDefinition = VariableDefinitionType Type Identifier (Maybe Element)
                        | VariableAssignment Identifier Element
                        deriving (Show)


data ArrayDefinition = ArrayDefinitionType Type Identifier (Maybe Element)
                        | ArrayAssignment Identifier Element
                        deriving (Show)

identifierParser :: Parser Identifier
identifierParser = do
  first <- letter
  rest <- many (letter <|> digit <|> char '_')
  return (Identifier (first:rest))

elementParser :: Parser Element
elementParser = choice [LiteralElement <$> literalParser,
                        IdentifierElement <$> identifierParser]

elementListParser :: Parser Element
elementListParser = ArrayElement <$> arrayParser

arrayParser :: Parser [Element]
arrayParser = between (char '[' >> spaces) (spaces >> char ']') (elementParser `sepBy` (char ',' >> spaces))

parseIdentifier :: String -> Either ParseError Identifier
parseIdentifier input = parse identifierParser "" input

typeParser :: Parser Type
typeParser = choice [string "int" >> return IntType,
                     string "char" >> return CharType,
                     string "boolean" >> return BooleanType,
                     string "string" >> return StringType,
                     string "function" >> return FunctionType,
                     ArrayType <$> (spaces >> char '[' >> spaces >> typeParser <* spaces <* char ']')]

variableDefinitionParser :: Parser VariableDefinition
variableDefinitionParser = choice [variableDefinitionTypeParser,
                                   variableAssignmentParser]

variableDefinitionTypeParser :: Parser VariableDefinition
variableDefinitionTypeParser = do
  varType <- typeParser
  spaces
  varIdentifier <- identifierParser
  spaces
  assignment <- optionMaybe (char '=' >> spaces >> elementParser)
  _ <- char ';'
  return (VariableDefinitionType varType varIdentifier assignment)

variableAssignmentParser :: Parser VariableDefinition
variableAssignmentParser = do
  varIdentifier <- identifierParser
  spaces
  assignment <- char '=' >> spaces >> elementParser
  return (VariableAssignment varIdentifier assignment)

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
  return (ArrayAssignment varIdentifier assignment)
