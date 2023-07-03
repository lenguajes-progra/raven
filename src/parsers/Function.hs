module Function where

import Grammar
import Type
import Text.Parsec
import Text.Parsec.String(Parser)

parameterParser :: Parser (Type, Identifier)
parameterParser = (,) <$> typeParserArray <* spaces <*> identifierParser

parametersParser :: Parser Parameters
parametersParser = Parameters <$> parameterParser `sepBy` (char ',' *> spaces)
