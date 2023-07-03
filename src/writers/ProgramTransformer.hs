module ProgramTransformer where

import FunctionDefinitionTemplate
import Grammar

programTransformer :: Program -> String
programTransformer (Program (FuncDefList fdl)) =
  concatMap functionDefinitionTransformer fdl

main2 :: IO ()
main2 = putStrLn "hello :: Int -> Int -> Int\nhello a b = ab\n\twhere\n\t\tab :: Int\nab = 2\nnext = if a==b then 2 else 1\nhello2 ::  -> Bool\nhello2  = True\n\twhere\n\t\tb :: Bool\nb = True\n"