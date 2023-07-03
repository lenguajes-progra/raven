module FunctionDefinitionTemplate where

import Data.List
import ExpressionTemplate
import Grammar
import LiteralTypeTemplate
import StatementTransformer

-- functionTypeTemplate :: (String, String) -> [String] -> String
-- functionTypeTemplate (typ, identifier) parameters = identifier ++ " :: " ++ intercalate " -> " parameters ++ " -> " ++ typ

-- functionBodyTemplate :: String -> [String] -> [String] -> String -> String
-- functionBodyTemplate identifier parameters block expression = identifier ++ " " ++ intercalate " " parameters ++ " = " ++ if length block /= 0 then expression ++ blockTemplate block else expression

-- functionDefinitionTemplate :: (String, String) -> [String] -> String -> [String] -> [String] -> String -> String
-- functionDefinitionTemplate typIdentifier parametersType identifier parameters block expression = functionTypeTemplate typIdentifier parametersType ++ "\n" ++ functionBodyTemplate identifier parameters block expression

blockTemplate :: [String] -> String
blockTemplate statements = "\n\twhere\n\t\t" ++ intercalate "\n\t\t" statements

ifStatementBlockTemplate :: [String] -> String
ifStatementBlockTemplate (varWithoutAssignment : ifstatement : _) = splitString ' ' varWithoutAssignment ++ " = " ++ ifstatement ++ "\n"

splitString :: Char -> String -> String
splitString _ "" = ""
splitString delimiter (x : xs) = if x == delimiter then xs else splitString delimiter xs

-- functionTransformer :: FunctionDefinition -> (String, [String], String)
-- functionTransformer (FuncDefinition typ identifier parameters block expression) = (identifierTransformer identifier, parametersTransformer parameters, expressionTransformer expression)

functionDefinitionTransformer :: FunctionDefinition -> String
functionDefinitionTransformer (FuncDefinition typ identifier parameters block expression) = identifierTransformer identifier ++ " :: " ++ intercalate " -> " (parametersTypeTransformer parameters) ++ " -> " ++ typeTransformer typ ++ "\n" ++ identifierTransformer identifier ++ " " ++ intercalate " " (parametersIdentifierTransformer parameters) ++ " = " ++ if blockTransformer block /= "" then expressionTransformer expression ++ "\n\twhere\n" ++ blockTransformer block else expressionTransformer expression
functionDefinitionTransformer _ = ""

parametersIdentifierTransformer :: Parameters -> [String]
parametersIdentifierTransformer (Parameters []) = []
parametersIdentifierTransformer (Parameters ((_, identifierParameter) : ps)) = identifierTransformer identifierParameter : parametersIdentifierTransformer (Parameters ps)

parametersTypeTransformer :: Parameters -> [String]
parametersTypeTransformer (Parameters []) = []
parametersTypeTransformer (Parameters ((typeParameter, _) : ps)) = typeTransformer typeParameter : parametersTypeTransformer (Parameters ps)

blockTransformer :: Block -> String
blockTransformer (Block []) = ""
blockTransformer (Block ((VariableDefinition (VariableDefinitionWithoutAssignment _ ident)) : (IfStat is) : _)) = identifierTransformer ident ++ " = " ++ ifTransformer is ++ "\n"
blockTransformer (Block (statement : statements)) = "\t\t" ++ statementTransformer statement ++ blockTransformer (Block statements)

blockIfVerificationTransformer :: Block -> Bool
blockIfVerificationTransformer (Block ((VariableDefinition (VariableDefinitionWithoutAssignment x y)) : (IfStat (IfStatement a b c)) : statements)) = True
blockIfVerificationTransformer _ = False

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
  where
    result = if a > b then result2 else "menor"
    result2 = "mayor"

sum' :: Int -> Int -> Bool
sum' a b = c
  where
    c = a > b

list1 :: [Int]
list1 = a
  where
    a = [1, 2, 3]

com :: Int -> Int -> Bool
com a b = e
  where
    c = a > b
    d = b > a
    e = c && d

function :: Int -> Int -> String
function a b = result
  where
    result = if a > b then "mayor" else "menor"

main :: IO ()
main = do
  putStrLn "sum :: Int -> Int -> Bool\nsum a b = c\n\twhere c = a > b"
  putStrLn "sum :: Int -> Int -> Bool\nsum a b = result\n\twhere string result\n\t\tif (a>b) {result = \"mayor\"} else {result = \"menor\"}"
  putStrLn "function :: Int -> Int -> Bool\nfunction a b = c\n\twhere\n\t\tc :: Bool\nc = a>b\n"
  putStrLn "function :: Int -> Int -> String\nfunction a b = result\n\twhere\n\t\tresult = if a>b then \"mayor\" else \"menor\""
  putStrLn "function :: Int -> Int -> Bool\nfunction a b = e\n\twhere\n\t\tc :: Bool\nc = a>b\n\t\td :: Bool\nd = b>a\n\t\te :: Bool\ne = c&&d\n"
