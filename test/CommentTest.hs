module CommentTest where

import Comment
import Grammar
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import Control.Exception (catch)

--Tests For Line Comments
testParseCommentLine :: Assertion
testParseCommentLine = assertEqual "testCommentLine" (parse parseCommentLine "" "//This is a comment line") (Right (CommentLine "This is a comment line"))

--Tests For Line Comments - Left Cases
testCommentLineLineBreakCase :: Assertion
testCommentLineLineBreakCase = assertEqual "testCommentLineLineBreakCase" (commentParser "//This is a comment line\n") (Left (ErrorType Syntax))

testCommentLineWrongStartOfLineCase :: Assertion
testCommentLineWrongStartOfLineCase = assertEqual "testCommentLineWrongStartOfLine" (commentParser "/This is a comment line") (Left (ErrorType Syntax))

--Tests For Block Comments
testParseCommentBlock :: Assertion
testParseCommentBlock = assertEqual "testCommentBlock" (parse parseCommentBlock "" "/*This is a comment block*/") (Right (CommentBlock "This is a comment block"))

testParseCommentBlockLineBreak :: Assertion
testParseCommentBlockLineBreak = assertEqual "testCommentBlockLineBreakCase" (parse parseCommentBlock "" "/*This is a comment block \n other line*/") (Right (CommentBlock "This is a comment block \n other line"))

--Tests For Block Comments  Left Cases
testCommentBlockWithoutFinalToken :: Assertion
testCommentBlockWithoutFinalToken = assertEqual "testCommentBlockWithoutFinalToken" (commentParser "/*This is a comment block") (Left (ErrorType Syntax))

testCommentBlockWrongStartOfLineCase :: Assertion
testCommentBlockWrongStartOfLineCase = assertEqual "testCommentBlockWrongStartOfLineCase" (commentParser "/This is a comment line*/") (Left (ErrorType Syntax))

commentTests :: TestTree
commentTests = testGroup "Buider Comments Tests" [
    testCase "testCommentLine" testParseCommentLine,
    testCase "testCommentLineLineBreakCase" testCommentLineLineBreakCase,
    testCase "testCommentLineWrongStartOfLine" testCommentLineWrongStartOfLineCase,
    testCase "testCommentBlock" testParseCommentBlock,
    testCase "testCommentBlockLineBreakCase" testParseCommentBlockLineBreak,
    testCase "testCommentBlockWithoutFinalToken" testCommentBlockWithoutFinalToken,
    testCase "testCommentBlockWrongStartOfLineCase" testCommentBlockWrongStartOfLineCase]