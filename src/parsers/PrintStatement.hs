module PrintStatement where

import Expression (Expression)
import Type
import Literal

data PrintStatement = PrintStatement Literal
                    deriving (Show)

executePrintStatement :: PrintStatement -> IO ()
executePrintStatement (PrintStatement literal) = print literal