module Error where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative hiding (many, optional, (<|>))

data Error = ErrorType ErrorType deriving (Show)

data ErrorType = Syntax
               | Type
               | FunctionType
               | AssignType
               deriving (Show)

errorParser :: Parser Error
errorParser = ErrorType <$> errorTypeParser

errorTypeParser :: Parser ErrorType
errorTypeParser = syntaxError <|> typeError <|> functionTypeError <|> assignTypeError

syntaxError :: Parser ErrorType
syntaxError = Syntax <$ string "Syntax" <* many1 space <* string "error" <* many1 space

typeError :: Parser ErrorType
typeError = Type <$ string "Type" <* many1 space <* string "error" <* many1 space

functionTypeError :: Parser ErrorType
functionTypeError = FunctionType <$ string "Function Type" <* many1 space <* string "error" <* many1 space

assignTypeError :: Parser ErrorType
assignTypeError = AssignType <$ string "Assign Type" <* many1 space <* string "error" <* many1 space