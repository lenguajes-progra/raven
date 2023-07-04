module ProgramTransformer where

import FunctionDefinitionTemplate
import Grammar
import Program
import Text.Parsec

programTransformer :: Program -> String
programTransformer (Program (FuncDefList fdl)) =
  "module Output where\nimport Data.Bits\n\n"
    ++ concatMap (`functionDefinitionTransformer` fdl) fdl
programTransformer _ = ""

textWriter :: IO ()
textWriter = do
  fileText <- readFile "../resources/input.rav"
  ( case parse programParser "../resources/input.rav" fileText of
      Right r -> case r of
        Right r' -> do
          print r'
          let code = programTransformer r'
          putStrLn code
          writeFile "../resources/Output.hs" code
        Left e -> print e
      Left e -> print e
    )