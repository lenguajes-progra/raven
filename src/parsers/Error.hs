module Error where

import Grammar
import Text.Parsec
import Text.Parsec.String (Parser)

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
