module PrintStatementTemplate where

import Grammar

printStatementTemplate :: String -> String
printStatementTemplate exp = "main :: IO()" ++ "\n" ++ "main = print" ++ "(" ++ exp ++ ")"

printStatementParserTransformer :: PrintStatement -> String
printStatementParserTransformer (PrintStatement exp) = expressionTransformer exp

literalTransformer :: Literal -> String
literalTransformer exp = case exp of
  IntegerLiteral a -> show a
  CharacterLiteral a -> show a
  StringLiteral a -> show a
  BooleanLiteral a -> show a

expressionTransformer :: Expression -> String
expressionTransformer (Literal literalExp) = literalTransformer literalExp