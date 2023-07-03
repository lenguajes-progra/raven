module Comment where

import Grammar
import Text.Parsec
import Text.Parsec.String(Parser)

commentParser :: String -> Either Error Comment
commentParser input = case parse commentParser' "" input of
  Left _ -> Left (ErrorType Syntax)
  Right comment -> Right comment

commentParser' :: Parser Comment
commentParser' = try parseCommentLine <|> parseCommentBlock

parseCommentLine :: Parser Comment
parseCommentLine = CommentLine <$> (string "//" *> manyTill anyChar (try newline <|> (eof >> return '\n')))

parseCommentBlock :: Parser Comment
parseCommentBlock = CommentBlock <$> (string "/*" *> many (noneOf "*") <* string "*/")