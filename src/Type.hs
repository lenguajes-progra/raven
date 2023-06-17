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
variableDefinitionParser = variableDefinitionTypeParser
                        <|> variableAssignmentParser

variableDefinitionTypeParser :: Parser VariableDefinition
variableDefinitionTypeParser = VariableDefinitionType
        <$> typeParser <* spaces
        <*> identifierParser <* spaces
        <*> optionMaybe (char '=' *> spaces *> elementParser)
        <* char ';'

variableAssignmentParser :: Parser VariableDefinition
variableAssignmentParser = VariableAssignment
        <$> identifierParser <* spaces
        <*> (char '=' *> spaces *> elementParser)

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
