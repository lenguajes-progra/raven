module StatementTest where

import Literal
import Grammar
import Parsers
import Type
import Statement
import Test.Tasty
import Test.Tasty.HUnit ( testCase, assertEqual, Assertion )
import Text.Parsec
import Control.Exception (catch)

testParseExpressionBitExpLeftShift :: Assertion
testParseExpressionBitExpLeftShift = assertEqual "parseExpressionBitExpLeftShift" (parse parseExpression "" "1 << 10") (Right (BitExpression (BitOp (Literal (IntegerLiteral 1)) LeftShift (Literal (IntegerLiteral 10)))))

testParseExpressionBitExpNotBit :: Assertion
testParseExpressionBitExpNotBit = assertEqual "parseExpressionBitExpNotBit" (parse parseExpression "" "~10") (Right (BitExpression (BitNot (Literal (IntegerLiteral 10)))))

statementTests :: TestTree
statementTests = testGroup "Builder Statement Tests" [
    testCase "parseExpressionBitExpLeftShift" testParseExpressionBitExpLeftShift,
    testCase "parseExpressionBitExpNotBit" testParseExpressionBitExpNotBit
    ]