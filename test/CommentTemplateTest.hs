module CommentTemplateTest where

import CommentTemplate
import Control.Exception (catch)
import DataTransformer
import Grammar
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import GHC.IO.Exception (assertError)

testCommentTransformerLine :: Assertion
testCommentTransformerLine =
  assertEqual
  "testCommentTransformerLine"
  (commentTransformer (CommentLine "This is a comment line"))
  ("--This is a comment line")

testCommentTransformerBlock :: Assertion
testCommentTransformerBlock =
  assertEqual
  "testCommentTransformerBlock"
  (commentTransformer (CommentBlock "This is a comment block"))
  ("{-This is a comment block-}")

commentTemplateTest :: TestTree
commentTemplateTest =
  testGroup "Comment Template Test" [
    testCase "testCommentTransformerLine"
    testCommentTransformerLine,
    testCase "testCommentTransformerBlock"
    testCommentTransformerBlock
  ]