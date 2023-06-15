module Comment where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative hiding (many, optional, (<|>))

data Comment = CommentLine String
             | CommentBlock String
             deriving (Show)

commentParser :: Parser Comment
commentParser = try parseCommentLine <|> parseCommentBlock

parseCommentLine :: Parser Comment
parseCommentLine = CommentLine <$> (string "//" *> manyTill (noneOf "\n") eof)

parseCommentBlock :: Parser Comment
parseCommentBlock = CommentBlock <$> (string "/*" *> many (noneOf "*") <* string "*/")