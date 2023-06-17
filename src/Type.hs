module Type where

import Text.Parsec
import Text.Parsec.String
import Literal

data Identifier = Identifier String
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

data VariableDefinition = VariableDefinitionComplete Type Identifier (Either String Literal)
                         | VariableDefinitionWithoutAssignment Type Identifier
                         | VariableDefinitionWithAssignment Identifier Literal
                         deriving (Show)

data ArrayDefinition = ArrayDefinitionComplete Type Identifier (Either String ElementList)
                     | ArrayDefinitionWithoutAssignment Type Identifier
                     | ArrayDefinitionWithAssignment Identifier ElementList
                     deriving (Show)

data Parameters = Parameters [(Type, Identifier)]
                deriving (Show)

-- data FunctionDefinition = FuncDefinition Type Identifier Parameters Block Expression
--                         deriving (Show)

identifierParser :: Parser Identifier
identifierParser = Identifier <$> ((:) <$> letter <*> many (letter <|> digit <|> char '_'))

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
arrayDefinitionParser = try arrayDefinitionCompleteParser
                    <|> try arrayDefinitionWithoutAssignmentParser
                    <|> try arrayDefinitionWithAssignmentParser

arrayDefinitionCompleteParser :: Parser ArrayDefinition
arrayDefinitionCompleteParser = ArrayDefinitionComplete
        <$> typeParser <* spaces
        <*> identifierParser <* spaces
        <*> (char '=' *> spaces *> ((Left <$> string "error") <|> (Right <$> elementListParser)))

arrayDefinitionWithoutAssignmentParser :: Parser ArrayDefinition
arrayDefinitionWithoutAssignmentParser = ArrayDefinitionWithoutAssignment
              <$> typeParser <* spaces
              <*> identifierParser <* char ';'

arrayDefinitionWithAssignmentParser :: Parser ArrayDefinition
arrayDefinitionWithAssignmentParser = ArrayDefinitionWithAssignment
        <$> identifierParser <* spaces
        <*> (char '=' *> spaces *> elementListParser)
        <* char ';'

parameterParser :: Parser (Type, Identifier)
parameterParser = (,) <$> typeParser <* spaces <*> identifierParser

parametersParser :: Parser Parameters
parametersParser = Parameters <$> parameterParser `sepBy` (char ',' *> spaces)
