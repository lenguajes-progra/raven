module PrintStatement where

import Grammar

executePrintStatement :: PrintStatement -> IO ()
executePrintStatement (PrintStatement literal) = print literal