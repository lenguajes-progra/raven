module Parsers  where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = whitespace *> p <* whitespace

symbol :: Char -> Parser ()
symbol c = void $ lexeme $ char c

betweenParens :: Parser a -> Parser a
betweenParens p = symbol '(' *> p <* symbol ')'