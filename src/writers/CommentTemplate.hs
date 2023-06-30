module CommentTemplate where

import Grammar

commentTemplate :: String -> String
commentTemplate exp = exp

commentTransformer :: Comment -> String
commentTransformer exp = case exp of
  CommentLine str -> "--" ++ str
  CommentBlock str -> "{-" ++ str ++ "-}"
