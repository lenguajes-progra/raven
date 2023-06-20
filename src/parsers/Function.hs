module Function where

import Grammar
import Literal
import Parsers
import Text.Parsec
import Text.Parsec.String
import Type

parameterParser :: Parser (Type, Identifier)
parameterParser = (,) <$> typeParser <* spaces <*> identifierParser

parametersParser :: Parser Parameters
parametersParser = Parameters <$> parameterParser `sepBy` (char ',' *> spaces)
