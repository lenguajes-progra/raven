module IfTest where

import Literal
import Grammar
import Parsers
import Type
import Expression
import If
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import Control.Exception (catch)

testIfStatement :: Assertion
testIfStatement = assertEqual "parseIfStatement" (parse ifStatementParser "" "if (false > true) block else block end") (Right (IfStatement (NumericExpression (NumericOp (Literal (BooleanLiteral False)) GreaterThan (Literal (BooleanLiteral True)))) "block" "block"))

ifTests :: TestTree
ifTests = testGroup "Builder Function Tests" [
    testCase "parseIfStatement" testIfStatement]