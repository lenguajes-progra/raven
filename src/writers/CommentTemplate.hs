module CommentTemplate where

import Grammar

commetTemplate :: String -> String
commetTemplate exp = exp

commentTransformer :: Comment -> String
commentTransformer exp = case exp of
  CommentLine str -> "--" ++ str
  CommentBlock str -> "{-" ++ str ++ "-}"