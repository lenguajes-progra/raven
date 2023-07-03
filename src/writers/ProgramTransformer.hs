module ProgramTransformer where

import Grammar
import Program
import FunctionDefinitionTemplate
import Text.Parsec

programTransformer :: Program -> String
programTransformer (Program (FuncDefList fdl)) =
  concatMap functionDefinitionTransformer fdl

main2 :: IO ()
main2 = do
  fileText <- readFile "../resources/input.rav"
  putStrLn
    ( case parse programParser "../resources/input.rav" fileText of
        Right r -> case r of
          Right r' -> programTransformer r'
          Left e -> show e
        Left e -> show e
    )