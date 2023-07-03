module FunctionDefinitionTemplate where

import Grammar
import LiteralTypeTemplate
import Data.List
import ExpressionTemplate
import StatementTransformer

functionTypeTemplate :: (String, String) -> [String] -> String
functionTypeTemplate (typ, identifier) parameters = identifier ++ " :: " ++ intercalate " -> " parameters ++ " -> " ++ typ

functionBodyTemplate :: String -> [String] -> [String] -> String -> String
functionBodyTemplate identifier parameters block expression = identifier ++ " " ++ intercalate " " parameters ++ " = " ++ if length block /= 0 then expression ++ blockTemplate block else expression

functionDefinitionTemplate :: (String, String) -> [String] -> String -> [String] -> [String] -> String -> String
functionDefinitionTemplate typIdentifier parametersType identifier parameters block expression = functionTypeTemplate typIdentifier parametersType ++ "\n" ++ functionBodyTemplate identifier parameters block expression

blockTemplate :: [String] -> String
blockTemplate statements = "\n\twhere " ++ intercalate "\n\t\t" statements

ifStatementBlockTemplate :: [String] -> String
ifStatementBlockTemplate (varWithoutAssignment : ifstatement : _) = splitString ' ' varWithoutAssignment ++ " = " ++ ifstatement

splitString :: Char -> String -> String
splitString _ "" = ""
splitString delimiter (x:xs) = if x == delimiter then xs else splitString delimiter xs

functionTransformer :: FunctionDefinition -> (String, [String], String)
functionTransformer (FuncDefinition typ identifier parameters block expression) = (identifierTransformer identifier, parametersTransformer parameters, expressionTransformer expression)

parametersTransformer :: Parameters -> [String]
parametersTransformer (Parameters []) = []
parametersTransformer (Parameters ((_, identifierParameter):ps)) = identifierTransformer identifierParameter:parametersTransformer (Parameters ps)

blockTransformer :: Block -> String
blockTransformer (Block []) = ""
blockTransformer (Block (statement:statements)) = statementTransformer statement ++ blockTransformer (Block statements)

{-
boolean sum (int a, int b) {
  bool c = a > b
}
return c
end

[int] list () {
  [int] a = [1,2,3]
}
return a
end

boolean com (int a, int b) {
  bool c = a > b
  bool d = b > a
  bool e = c && d
}
return e
end

string isGreater (int a, int b) {
  string result;
  if(a > b) {
    int result2 = "mayor"
  } else {
    result = "menor"
  }
  end
}
return result
end

-}

isGreater :: Int -> Int -> String
isGreater a b = result
  where result = if a > b then result2 else "menor"
        result2 = "mayor"

sum' :: Int -> Int -> Bool
sum' a b = c
  where c = a > b

list1 :: [Int]
list1 = a
  where a = [1,2,3]

com :: Int -> Int -> Bool
com a b = e
  where c = a > b
        d = b > a
        e = c && d

main :: IO()
main = do
  putStrLn "sum :: Int -> Int -> Bool\nsum a b = c\n\twhere c = a > b"
  putStrLn "sum :: Int -> Int -> Bool\nsum a b = result\n\twhere string result\n\t\tif (a>b) {result = \"mayor\"} else {result = \"menor\"}"
