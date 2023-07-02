module LoopTemplate where

import Grammar
import ExpressionTemplate

loopDefTemplate :: String
loopDefTemplate = "for :: a -> (a -> Bool) -> (a -> a) -> a" ++ "\n" ++ loopBodyTemplate

loopBodyTemplate :: String
loopBodyTemplate = "for value condition action = if condition value then for (action value) condition action else value"

loopTransformer :: ForStatement -> String
loopTransformer (ForStatement value condition action) = "(for " ++ expressionTransformer value
  ++ createLambda (expressionTransformer value) (expressionTransformer condition) 
  ++ createLambda (expressionTransformer value) (expressionTransformer action) ++ ")"

for :: a -> (a -> Bool) -> (a -> a) -> a
for value condition action = if condition value then for (action value) condition action else value

createLambda :: String -> String -> String
createLambda value expr = " (\\x -> " ++ replaceSequence value "x" expr ++ ")"

replaceSequence :: String -> String -> String -> String
replaceSequence sequence replacement expr = 
    case expr of
        [] -> []
        x:xs ->
            if take (length sequence) expr == sequence
                then replacement ++ replaceSequence sequence replacement (drop (length sequence) expr)
                else x : replaceSequence sequence replacement xs


