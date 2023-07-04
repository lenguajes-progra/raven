module LoopTemplate where

import ExpressionTemplate
import Grammar

loopDefTemplate :: String
loopDefTemplate = "for :: a -> (a -> Bool) -> (a -> a) -> a" ++ "\n" ++ loopBodyTemplate

loopBodyTemplate :: String
loopBodyTemplate = "for value condition action = if condition value then for (action value) condition action else value"

loopTransformer :: ForStatement -> [FunctionDefinition] -> String
loopTransformer (ForStatement value condition action) fd =
  "(for "
    ++ expressionTransformer value fd
    ++ createLambda (expressionTransformer value fd) (expressionTransformer condition fd)
    ++ createLambda (expressionTransformer value fd) (expressionTransformer action fd)
    ++ ")"

for :: a -> (a -> Bool) -> (a -> a) -> a
for value condition action = if condition value then for (action value) condition action else value

createLambda :: String -> String -> String
createLambda value expr = " (\\x -> " ++ replaceSequence value "x" expr ++ ")"

replaceSequence :: String -> String -> String -> String
replaceSequence sequence replacement expr =
  case expr of
    [] -> []
    x : xs ->
      if take (length sequence) expr == sequence
        then replacement ++ replaceSequence sequence replacement (drop (length sequence) expr)
        else x : replaceSequence sequence replacement xs
