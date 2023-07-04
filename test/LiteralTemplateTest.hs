module LiteralTemplateTest where

import LiteralTypeTemplate
import Grammar
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

testLiteralTransformerInt :: Assertion
testLiteralTransformerInt = assertEqual "literalTransformerInt" (literalTransformer (IntegerLiteral 2)) "2"

testLiteralTransformerBool :: Assertion
testLiteralTransformerBool = assertEqual "literalTransformerBool" (literalTransformer (BooleanLiteral True)) "True"

testLiteralTransformerChar :: Assertion
testLiteralTransformerChar = assertEqual "literalTransformerChar" (literalTransformer (CharacterLiteral 'a')) "'a'"

testLiteralTransformerString :: Assertion
testLiteralTransformerString = assertEqual "literalTransformerString" (literalTransformer (StringLiteral "a")) "\"a\""

literalTemplateTests :: TestTree
literalTemplateTests =
  testGroup
    "Builder LiteralTemplate Tests"
    [ 
      testCase "literalTransformerInt" testLiteralTransformerInt,
      testCase "literalTransformerBool" testLiteralTransformerBool,
      testCase "literalTransformerChar" testLiteralTransformerChar,
      testCase "literalTransformerString" testLiteralTransformerString
    ]