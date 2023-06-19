module ExpressionTest where

import Literal
import Grammar
import Parsers
import Type
import Expression
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import Control.Exception (catch)

testParseExpressionNumericExp :: Assertion
testParseExpressionNumericExp = assertEqual "parseExpressionNumericExp" (parse parseExpression "" "1 > 2") (Right (NumericExpression (NumericOp (Literal (IntegerLiteral 1)) GreaterThan (Literal (IntegerLiteral 2)))))

testParseExpressionLogicalOrExp :: Assertion
testParseExpressionLogicalOrExp = assertEqual "parseExpressionLogicalOrExp" (parse parseExpression "" "true || false") (Right (LogicalExpression (LogicOp (Literal (BooleanLiteral True)) Or (Literal (BooleanLiteral False)))))

testParseExpressionLogicalAndExp :: Assertion
testParseExpressionLogicalAndExp = assertEqual "parseExpressionLogicalAndExp" (parse parseExpression "" "true && true") (Right (LogicalExpression (LogicOp (Literal (BooleanLiteral True)) And (Literal (BooleanLiteral True)))))

testParseExpressionLogicalNotExp :: Assertion
testParseExpressionLogicalNotExp = assertEqual "parseExpressionLogicalNotExp" (parse parseExpression "" "!false") (Right (LogicalExpression (LogicNot (Literal (BooleanLiteral False)))))

testParseExpressionBitExp :: Assertion
testParseExpressionBitExp = assertEqual "parseExpressionBitExp" (parse parseExpression "" "1 >> 10") (Right (BitExpression (BitOp (Literal (IntegerLiteral 1)) RightShift (Literal (IntegerLiteral 10)))))

expressionTests :: TestTree
expressionTests = testGroup "Builder Expression Tests" [
    testCase "parseExpressionNumericExp" testParseExpressionNumericExp,
    testCase "parseExpressionLogicalOrExp" testParseExpressionLogicalOrExp,
    testCase "parseExpressionLogicalAndExp" testParseExpressionLogicalAndExp,
    testCase "parseExpressionLogicalNotExp" testParseExpressionLogicalNotExp,
    testCase "parseExpressionBitExp" testParseExpressionBitExp]