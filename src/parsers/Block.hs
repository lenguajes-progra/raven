module Block where

import Statement
import Grammar
import Text.Parsec
import Text.Parsec.String

blockParse :: Parser Block
blockParse = Block <$> statementParse `sepBy` (char ';' *> spaces)