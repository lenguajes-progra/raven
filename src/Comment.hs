module Comment where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative hiding (many, optional)

data Comment = CommentLine String
             | CommentBlock String
             deriving (Show)

commentParser :: Parser Comment
commentParser = choice [try parseCommentLine, parseCommentBlock]

parseCommentLine :: Parser Comment
parseCommentLine = do
    string "//"
    value <- manyTill (noneOf "\n") eof
    return (CommentLine value)

parseCommentBlock :: Parser Comment
parseCommentBlock = do
    string "/*"
    value <- many (noneOf "*")
    string "*/"
    return (CommentBlock value)