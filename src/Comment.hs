module Comment where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative hiding (many, optional)
import qualified Text.Parsec.Char as PC
import Text.ParserCombinators.ReadP (choice)

data Comment = CommentLine String
             | CommentBlock String
             deriving Show


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
    return (CommentLine value)