module Parsers (regularParse) where

import Text.Parsec
import Text.Parsec.String (Parser)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""
