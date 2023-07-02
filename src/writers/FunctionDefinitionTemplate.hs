module FunctionDefinitionTemplate where

import Grammar
import LiteralTypeTemplate
import Data.List

functionTypeTemplate :: (String, String) -> [String] -> [String] -> String
functionTypeTemplate (typ, identifier) parameters block = identifier ++ " :: " ++ intercalate " -> " parameters ++ " -> " ++ typ 

functionDefinitionTemplate :: String -> [String] -> [String] -> String -> String
functionDefinitionTemplate identifier parameters block expression = identifier ++ " " ++ intercalate " " parameters ++ " = " ++ if length block /= 0 then expression ++ blockTemplate block else expression

blockTemplate :: [String] -> String
blockTemplate statements = "\n\twhere " ++ intercalate "\n\t\t" statements

functionTransformer :: FunctionDefinition -> (String, [String], String)
functionTransformer (FuncDefinition typ identifier parameters block expression) = (identifierTransformer identifier, parametersTransformer parameters, show expression)

parametersTransformer :: Parameters -> [String]
parametersTransformer (Parameters []) = []
parametersTransformer (Parameters ((_, identifierParameter):ps)) = identifierTransformer identifierParameter:parametersTransformer (Parameters ps)

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
    result= "mayor"
  } else {
    result = "menor"
  }
  end
}
return result
end

-}

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

isGreater :: Int -> Int -> String
isGreater a b = result
  where result = if a > b then "mayor" else "menor"

main :: IO()
main = putStrLn "sum a b = e\n\twhere c = a > b\n\t\td = b > a\n\t\te = c && d"
