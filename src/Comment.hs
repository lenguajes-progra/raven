module Comment where

data Comment = CommentLine String
             | CommentBlock String
             deriving Show

data Line = Line

data Block = StartBlock | EndBlock