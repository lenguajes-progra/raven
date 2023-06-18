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
syntaxError = Syntax <$ string "Syntax" <* many1 space <* string "error" <* (many space <|> string ";")

typeError :: Parser ErrorType
typeError = Type <$ string "Type" <* many1 space <* string "error" <* (many space <|> string ";")

functionTypeError :: Parser ErrorType
functionTypeError = TypeFunction <$ string "Function Type" <* many1 space <* string "error" <* (many space <|> string ";")

assignTypeError :: Parser ErrorType
assignTypeError = AssignType <$ string "Assign Type" <* many1 space <* string "error" <* (many space <|> string ";")
