module VariableTemplateTest where

import VariableDefTemplate
import Grammar
import DataTransformer
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Text.Parsec (Consumed(Empty))

testVariableDefinitionTemplate :: Assertion
testVariableDefinitionTemplate = assertEqual "variableDefinitionTemplate" (variableDefinitionTemplate "var" "Int" "var * 2") "Int :: var\nInt = var * 2\n"

testVariableBodyTemplate :: Assertion
testVariableBodyTemplate = assertEqual "variableBodyTemplate" (variableBodyTemplate "var" "var * 2") "var = var * 2"

testVariableWithoutAssignment :: Assertion
testVariableWithoutAssignment = assertEqual "variableWithoutAssignment" (variableWithoutAssignment "var") "\twhere var = "

testVariableDefBlockTemplate :: Assertion
testVariableDefBlockTemplate = assertEqual "variableDefBlockTemplate" (variableDefBlockTemplate "var" "var * 2") "\twhere var = var * 2"

testVariableExpressionTemplate :: Assertion
testVariableExpressionTemplate = assertEqual "variableExpressionTemplate" (variableExpressionTemplate "var * 2") "var * 2"

testVariableDefinitionTransformerComplete :: Assertion
testVariableDefinitionTransformerComplete = assertEqual "variableDefinitionTransformerComplete" (variableDefinitionTransformer (VariableDefinitionComplete IntType (Ident "ab") (Literal (IntegerLiteral 2))) []) (TriNode "Int" "ab" "2")

testVariableDefinitionTransformerWith :: Assertion
testVariableDefinitionTransformerWith = assertEqual "variableDefinitionTransformerWith" (variableDefinitionTransformer (VariableDefinitionWithoutAssignment IntType (Ident "result")) []) (TwiceNodeWithoutAssignment "Int" "result")

variableDefinitionTemplateTests :: TestTree
variableDefinitionTemplateTests =
  testGroup
    "Builder VariableDefinitionTemplate Tests"
    [ 
      testCase "variableDefinitionTemplate" testVariableDefinitionTemplate,
      testCase "variableBodyTemplate" testVariableBodyTemplate,
      testCase "variableWithoutAssignment" testVariableWithoutAssignment,
      testCase "variableDefBlockTemplate" testVariableDefBlockTemplate,
      testCase "variableExpressionTemplate" testVariableExpressionTemplate,
      testCase "variableDefinitionTransformerComplete" testVariableDefinitionTransformerComplete,
      testCase "variableDefinitionTransformerWith" testVariableDefinitionTransformerWith,
      testCase "variableDefinitionTemplate" testVariableDefinitionTemplate,
      testCase "variableDefinitionTemplate" testVariableDefinitionTemplate,
      testCase "variableDefinitionTemplate" testVariableDefinitionTemplate,
      testCase "variableDefinitionTemplate" testVariableDefinitionTemplate,
      testCase "variableDefinitionTemplate" testVariableDefinitionTemplate
    ]