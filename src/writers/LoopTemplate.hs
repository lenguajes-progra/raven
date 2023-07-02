module LoopTemplate where

import Grammar
import ExpressionTemplate

loopDefTemplate :: String
loopDefTemplate = "for :: a -> (a -> Bool) -> (a -> a) -> a" ++ "\n" ++ loopBodyTemplate

loopBodyTemplate :: String
loopBodyTemplate = "for value condition action = if condition value then for (action value) condition action else value"

loopTransformer :: ForStatement -> String
loopTransformer (ForStatement value condition action) = "(for " ++ expressionTransformer value ++ " ("
  ++ expressionTransformer condition ++ ") (" ++ expressionTransformer action ++ "))"

for :: a -> (a -> Bool) -> (a -> a) -> a
for value condition action = if condition value then for (action value) condition action else value


