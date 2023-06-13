module Type where

import Text.Parsec
import Text.Parsec.String (Parser)
import Literal

data Identifier = Identifier String
                deriving (Show)

data Element = LiteralElement Literal
             | IdentifierElement Identifier
             deriving (Show)

data Type = IntType
          | CharType
          | BooleanType
          | StringType
          | FunctionType
          deriving (Show)

data VariableDefinition = VariableDefinitionType Type Identifier (Maybe Element)
                        | VariableAssignment Identifier Element
                        deriving (Show)

identifierParser :: Parser Identifier
identifierParser = do
  first <- letter
  rest <- many (letter <|> digit <|> char '_')
  return (Identifier (first:rest))

elementParser :: Parser Element
elementParser = choice [LiteralElement <$> literalParser,
                        IdentifierElement <$> identifierParser]

parseIdentifier :: String -> Either ParseError Identifier
parseIdentifier input = parse identifierParser "" input

typeParser :: Parser Type
typeParser = choice [string "int" >> return IntType,
                     string "char" >> return CharType,
                     string "boolean" >> return BooleanType,
                     string "string" >> return StringType,
                     string "function" >> return FunctionType]

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
