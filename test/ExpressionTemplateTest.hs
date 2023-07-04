module ExpressionTemplateTest where

import ExpressionTemplate
import Control.Exception (catch)
import DataTransformer
import Grammar
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import GHC.IO.Exception (assertError)

testNumericExpGreater :: Assertion
testNumericExpGreater =
  assertEqual
  "testNumericExpGreate"
  (expressionTransformer (BooleanExpression (BooleanOp (Literal (IntegerLiteral 1)) GreaterThan (Literal (IntegerLiteral 2)))) []) ("1>2")

testNumericExpGreaterEq :: Assertion
testNumericExpGreaterEq =
  assertEqual
  "testNumericExpGreaterEq"
  (expressionTransformer (BooleanExpression (BooleanOp (Literal (IntegerLiteral 1)) GreatEqualThan (Literal (IntegerLiteral 2)))) []) ("1>=2")

testNumericExpLess :: Assertion
testNumericExpLess =
  assertEqual
  "testNumericExpLess"
  (expressionTransformer (BooleanExpression (BooleanOp (Literal (IntegerLiteral 1)) LessThan (Literal (IntegerLiteral 2)))) []) ("1<2")

testNumbericExpLessEq :: Assertion
testNumbericExpLessEq =
  assertEqual
  "testNumbericExpLessEq"
  (expressionTransformer (BooleanExpression (BooleanOp (Literal (IntegerLiteral 1)) LessEqualThan (Literal (IntegerLiteral 2)))) []) ("1<=2")

testNumericExpDifferent :: Assertion
testNumericExpDifferent =
  assertEqual
  "testNumericExpDifferent"
  (expressionTransformer (BooleanExpression (BooleanOp (Literal (IntegerLiteral 1)) NotEqual (Literal (IntegerLiteral 2)))) []) ("1/=2")

testNumericExpEqual :: Assertion
testNumericExpEqual =
  assertEqual
  "testNumericExpEqual"
  (expressionTransformer (BooleanExpression (BooleanOp (Literal (IntegerLiteral 1)) Equal (Literal (IntegerLiteral 2)))) []) ("1==2")

testLogicalOrExp :: Assertion
testLogicalOrExp =
  assertEqual
  "testLogicalOrExp"
  (expressionTransformer (LogicalExpression (LogicOp (Literal (BooleanLiteral True)) Or (Literal (BooleanLiteral False)))) []) ("True||False")

testLogicalAndExp :: Assertion
testLogicalAndExp =
  assertEqual
  "testLogicalAndExp"
  (expressionTransformer (LogicalExpression (LogicOp (Literal (BooleanLiteral True)) And (Literal (BooleanLiteral True)))) []) ("True&&True")

testLogicalNotExp :: Assertion
testLogicalNotExp =
  assertEqual
  "testLogicalNotExp"
  (expressionTransformer (LogicalExpression (LogicNot (Literal (BooleanLiteral False)))) []) ("(not False)")

testBitExpAnd :: Assertion
testBitExpAnd =
  assertEqual
  "testBitExpAnd"
  (expressionTransformer (BitExpression (BitOp (Literal (IntegerLiteral 1)) AndBit (Literal (IntegerLiteral 10)))) []) ("1.&.10")

testBitExpOr :: Assertion
testBitExpOr =
  assertEqual
  "testBitExpOr"
  (expressionTransformer (BitExpression (BitOp (Literal (IntegerLiteral 1)) OrBit (Literal (IntegerLiteral 10)))) []) ("1.|.10")

testBitExpXor :: Assertion
testBitExpXor = 
  assertEqual
  "testBitExpXor"
  (expressionTransformer (BitExpression (BitOp (Literal (IntegerLiteral 1)) XorBit (Literal (IntegerLiteral 10)))) []) ("1^10")

testBitExpRightShift :: Assertion
testBitExpRightShift = 
  assertEqual
  "testBitExpRightShift"
  (expressionTransformer (BitExpression (BitOp (Literal (IntegerLiteral 1)) RightShift (Literal (IntegerLiteral 10)))) []) ("1`shiftR`10")

testBitExpLeftShift :: Assertion
testBitExpLeftShift =
  assertEqual
  "testBitExpLeftShift"
  (expressionTransformer (BitExpression (BitOp (Literal (IntegerLiteral 1)) LeftShift (Literal (IntegerLiteral 10)))) []) ("1`shiftL`10")

testBitExpNotBit :: Assertion
testBitExpNotBit = 
  assertEqual
  "testBitExpNotBit"
  (expressionTransformer (BitExpression (BitNot (Literal (IntegerLiteral 10)))) []) ("(complement 10)")

expressionTemplateTest :: TestTree
expressionTemplateTest =
  testGroup
  "Expression template test"[
    testCase "testNumericExpGreater"
    testNumericExpGreater,
    testCase "testNumericExpGreaterEq"
    testNumericExpGreaterEq,
    testCase "testNumericExpLess"
    testNumericExpLess,
    testCase "testNumbericExpLessEq"
    testNumbericExpLessEq,
    testCase "testNumericExpDifferent"
    testNumericExpDifferent,
    testCase "testNumericExpEqual"
    testNumericExpEqual,
    testCase "testLogicalOrExp"
    testLogicalOrExp,
    testCase "testLogicalAndExp"
    testLogicalAndExp,
    testCase "testLogicalNotExp"
    testLogicalNotExp,
    testCase "testBitExpAnd"
    testBitExpAnd,
    testCase "testBitExpOr"
    testBitExpOr,
    testCase "testBitExpXor"
    testBitExpXor,
    testCase "testBitExpRightShift"
    testBitExpRightShift,
    testCase "testBitExpLeftShift"
    testBitExpLeftShift,
    testCase "testBitExpNotBit"
    testBitExpNotBit
  ]
