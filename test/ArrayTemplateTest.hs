module ArrayTemplateTest where

import ArrayDefinitionTemplate
import Control.Exception (catch)
import DataTransformer
import Grammar
import LiteralTypeTemplate
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import GHC.IO.Exception (assertError)

testArrayDefinitionTransformerDefinitionComplete :: Assertion
testArrayDefinitionTransformerDefinitionComplete =
  assertEqual
    "testArrayDefinitionTransformerDefinitionComplete"
    (arrayDefinitionTransformer ((ArrayDefinitionComplete (ArrayType BooleanType) (Ident "arr") (Literals [BooleanLiteral True, BooleanLiteral False]))))
    (TriNode "[Bool]" "arr" "[True, False]")

testArrayDefinitionTransformerDefinitionWithOutAssignment :: Assertion
testArrayDefinitionTransformerDefinitionWithOutAssignment =
  assertEqual
    "testArrayDefinitionTransformerDefinitionWithOutAssignment"
    (arrayDefinitionTransformer ((ArrayDefinitionWithoutAssignment (ArrayType CharType) (Ident "arr"))))
    ((TwiceNodeWithoutAssignment "[Char]" "arr"))

testArrayDefinitionTransformerWithAssignment :: Assertion
testArrayDefinitionTransformerWithAssignment =
  assertEqual
    "testArrayDefinitionTransformerWithAssignment"
    (arrayDefinitionTransformer ((ArrayDefinitionComplete (ArrayType BooleanType) (Ident "arr") (Literals [BooleanLiteral True, BooleanLiteral False]))))
    (TriNode "[Bool]" "arr" "[True, False]")

arrayTemplateTest :: TestTree
arrayTemplateTest =
  testGroup
    "Builder Array Tests"
    [ testCase
      "testArrayDefinitionTransformerDefinitionComplete"
      testArrayDefinitionTransformerDefinitionComplete,
      testCase
      "testArrayDefinitionTransformerDefinitionWithOutAssignment"
      testArrayDefinitionTransformerDefinitionWithOutAssignment,
      testCase "testArrayDefinitionTransformerWithAssignment"
      testArrayDefinitionTransformerWithAssignment
    ]