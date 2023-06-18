module Error (Error (ErrorType), ErrorType (Syntax, Type, TypeFunction, AssignType), errorParser) where

import Control.Applicative hiding (many, optional, (<|>))
import Text.Parsec
import Text.Parsec.String (Parser)

data Error = ErrorType ErrorType deriving (Show)

data ErrorType = Syntax
               | Type
               | TypeFunction
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
functionTypeError = TypeFunction <$ string "Function Type" <* many1 space <* string "error" <* many1 space

assignTypeError :: Parser ErrorType
assignTypeError = AssignType <$ string "Assign Type" <* many1 space <* string "error" <* many1 space
