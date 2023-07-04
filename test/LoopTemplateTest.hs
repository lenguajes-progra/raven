module LoopTemplateTest where

import LoopTemplate
import Grammar
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

testLoopTransformerBool :: Assertion
testLoopTransformerBool = assertEqual "loopTransformerBool" (loopTransformer (ForStatement (Identifier (Ident "a")) (Literal (BooleanLiteral True)) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))))) "(for a (\\x -> True) (\\x -> x==b))"

testLoopTransformerInt :: Assertion
testLoopTransformerInt = assertEqual "loopTransformerInt" (loopTransformer (ForStatement (Identifier (Ident "a")) (Literal (IntegerLiteral 1)) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))))) "(for a (\\x -> 1) (\\x -> x==b))"

testLoopTransformerChar :: Assertion
testLoopTransformerChar = assertEqual "loopTransformerChar" (loopTransformer (ForStatement (Identifier (Ident "a")) (Literal (CharacterLiteral 'a')) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))))) "(for a (\\x -> 'x') (\\x -> x==b))"

testLoopTransformerString :: Assertion
testLoopTransformerString = assertEqual "loopTransformerString" (loopTransformer (ForStatement (Identifier (Ident "a")) (Literal (StringLiteral "a")) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))))) "(for a (\\x -> \"x\") (\\x -> x==b))"

loopTemplateTests :: TestTree
loopTemplateTests =
  testGroup
    "Builder LoopTemplate Tests"
    [ testCase "loopTransformerBool" testLoopTransformerBool,
      testCase "loopTransformerInt" testLoopTransformerInt,
      testCase "loopTransformerChar" testLoopTransformerChar,
      testCase "loopTransformerString" testLoopTransformerString
    ]