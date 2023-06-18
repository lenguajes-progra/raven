module Comment where

import Error
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative hiding (many, optional, (<|>))

data Comment = CommentLine String
             | CommentBlock String
             deriving (Show)

commentParser :: String -> Either Error Comment
commentParser input = case parse commentParser' "" input of
  Left _ -> Left (ErrorType Syntax)
  Right comment -> Right comment

commentParser' :: Parser Comment
commentParser' = try parseCommentLine <|> parseCommentBlock

parseCommentLine :: Parser Comment
parseCommentLine = CommentLine <$> (string "//" *> manyTill (noneOf "\n") eof)

parseCommentBlock :: Parser Comment
parseCommentBlock = CommentBlock <$> (string "/*" *> many (noneOf "*") <* string "*/")