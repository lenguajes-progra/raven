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

testParseExpressionNumericExpGreater :: Assertion
testParseExpressionNumericExpGreater = assertEqual "parseExpressionNumericExpGreater" (parse parseExpression "" "1 > 2") (Right (NumericExpression (NumericOp (Literal (IntegerLiteral 1)) GreaterThan (Literal (IntegerLiteral 2)))))

testParseExpressionNumericExpGreaterEq :: Assertion
testParseExpressionNumericExpGreaterEq = assertEqual "parseExpressionNumericExpGreaterEq" (parse parseExpression "" "1 >= 2") (Right (NumericExpression (NumericOp (Literal (IntegerLiteral 1)) GreatEqualThan (Literal (IntegerLiteral 2)))))

testParseExpressionNumericExpLess :: Assertion
testParseExpressionNumericExpLess = assertEqual "parseExpressionNumericExpLess" (parse parseExpression "" "1 < 2") (Right (NumericExpression (NumericOp (Literal (IntegerLiteral 1)) LessThan (Literal (IntegerLiteral 2)))))

testParseExpressionNumericExpLessEq :: Assertion
testParseExpressionNumericExpLessEq = assertEqual "parseExpressionNumericExpLessEq" (parse parseExpression "" "1 <= 2") (Right (NumericExpression (NumericOp (Literal (IntegerLiteral 1)) LessEqualThan (Literal (IntegerLiteral 2)))))

testParseExpressionNumericExpDifferent :: Assertion
testParseExpressionNumericExpDifferent = assertEqual "parseExpressionNumericExpDifferent" (parse parseExpression "" "1 != 2") (Right (NumericExpression (NumericOp (Literal (IntegerLiteral 1)) NotEqual (Literal (IntegerLiteral 2)))))

testParseExpressionNumericExpEqual :: Assertion
testParseExpressionNumericExpEqual = assertEqual "parseExpressionNumericExpEqual" (parse parseExpression "" "1 == 2") (Right (NumericExpression (NumericOp (Literal (IntegerLiteral 1)) Equal (Literal (IntegerLiteral 2)))))

testParseExpressionLogicalOrExp :: Assertion
testParseExpressionLogicalOrExp = assertEqual "parseExpressionLogicalOrExp" (parse parseExpression "" "true || false") (Right (LogicalExpression (LogicOp (Literal (BooleanLiteral True)) Or (Literal (BooleanLiteral False)))))

testParseExpressionLogicalAndExp :: Assertion
testParseExpressionLogicalAndExp = assertEqual "parseExpressionLogicalAndExp" (parse parseExpression "" "true && true") (Right (LogicalExpression (LogicOp (Literal (BooleanLiteral True)) And (Literal (BooleanLiteral True)))))

testParseExpressionLogicalNotExp :: Assertion
testParseExpressionLogicalNotExp = assertEqual "parseExpressionLogicalNotExp" (parse parseExpression "" "!false") (Right (LogicalExpression (LogicNot (Literal (BooleanLiteral False)))))

testParseExpressionBitExpAnd :: Assertion
testParseExpressionBitExpAnd = assertEqual "parseExpressionBitExpAnd" (parse parseExpression "" "1 & 10") (Right (BitExpression (BitOp (Literal (IntegerLiteral 1)) AndBit (Literal (IntegerLiteral 10)))))

testParseExpressionBitExpOr :: Assertion
testParseExpressionBitExpOr = assertEqual "parseExpressionBitExpOr" (parse parseExpression "" "1 | 10") (Right (BitExpression (BitOp (Literal (IntegerLiteral 1)) OrBit (Literal (IntegerLiteral 10)))))

testParseExpressionBitExpXor :: Assertion
testParseExpressionBitExpXor = assertEqual "parseExpressionBitExpXor" (parse parseExpression "" "1 ^ 10") (Right (BitExpression (BitOp (Literal (IntegerLiteral 1)) XorBit (Literal (IntegerLiteral 10)))))

testParseExpressionBitExpRightShift :: Assertion
testParseExpressionBitExpRightShift = assertEqual "parseExpressionBitExpRightShift" (parse parseExpression "" "1 >> 10") (Right (BitExpression (BitOp (Literal (IntegerLiteral 1)) RightShift (Literal (IntegerLiteral 10)))))

testParseExpressionBitExpLeftShift :: Assertion
testParseExpressionBitExpLeftShift = assertEqual "parseExpressionBitExpLeftShift" (parse parseExpression "" "1 << 10") (Right (BitExpression (BitOp (Literal (IntegerLiteral 1)) LeftShift (Literal (IntegerLiteral 10)))))

testParseExpressionBitExpNotBit :: Assertion
testParseExpressionBitExpNotBit = assertEqual "parseExpressionBitExpNotBit" (parse parseExpression "" "~10") (Right (BitExpression (BitNot (Literal (IntegerLiteral 10)))))

expressionTests :: TestTree
expressionTests = testGroup "Builder Expression Tests" [
    testCase "parseExpressionNumericExpGreater" testParseExpressionNumericExpGreater,
    testCase "parseExpressionNumericExpGreaterEq" testParseExpressionNumericExpGreaterEq,
    testCase "parseExpressionNumericExpLess" testParseExpressionNumericExpLess,
    testCase "parseExpressionNumericExpLessEq" testParseExpressionNumericExpLessEq,
    testCase "parseExpressionNumericExpDifferent" testParseExpressionNumericExpDifferent,
    testCase "parseExpressionNumericExpEqual" testParseExpressionNumericExpEqual,
    testCase "parseExpressionLogicalOrExp" testParseExpressionLogicalOrExp,
    testCase "parseExpressionLogicalAndExp" testParseExpressionLogicalAndExp,
    testCase "parseExpressionLogicalNotExp" testParseExpressionLogicalNotExp,
    testCase "parseExpressionBitExpAnd" testParseExpressionBitExpAnd,
    testCase "parseExpressionBitExpOr" testParseExpressionBitExpOr,
    testCase "parseExpressionBitExpXor" testParseExpressionBitExpXor,
    testCase "parseExpressionBitExpRightShift" testParseExpressionBitExpRightShift,
    testCase "parseExpressionBitExpLeftShift" testParseExpressionBitExpLeftShift,
    testCase "parseExpressionBitExpNotBit" testParseExpressionBitExpNotBit
    ]