module FunctionTemplateTest where

import FunctionDefinitionTemplate
import Control.Exception (catch)
import DataTransformer
import Grammar
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import GHC.IO.Exception (assertError)

testFunctionCharType :: Assertion
testFunctionCharType =
  assertEqual
  "testFunctionCharType"
  (functionDefinitionTransformer (FuncDefinition BooleanType (Ident "function") (Parameters [(IntType,Ident "a"),(IntType,Ident "b")]) (Block [VariableDefinition (VariableDefinitionComplete BooleanType (Ident "c") (BooleanExpression (BooleanOp (Identifier (Ident "a")) GreaterThan (Identifier (Ident "b")))))]) (Identifier (Ident "c"))))
  ("function :: Int -> Int -> Bool\nfunction a b = c\n\twhere\n\t\t\t\tc :: Bool\nc = a>b\n")

testFunctionBooleanType :: Assertion
testFunctionBooleanType = 
  assertEqual
  "testFunctionBooleanType"
  (functionDefinitionTransformer (FuncDefinition BooleanType (Ident "my_function") (Parameters []) (Block [VariableDefinition (VariableDefinitionWithAssignment (Ident "true") (Literal (BooleanLiteral False)))]) (Literal (BooleanLiteral True))))
  ("my_function :: Bool\nmy_function  = True\n\twhere\n\t\t\t\ttrue = False\n")

testFunctionStringType :: Assertion
testFunctionStringType = 
  assertEqual
  "testFunctionStringType"
  (functionDefinitionTransformer (FuncDefinition StringType (Ident "my_function") (Parameters []) (Block [VariableDefinition (VariableDefinitionWithoutAssignment StringType (Ident "a"))]) (Literal (StringLiteral "True"))))
  ("my_function :: String\nmy_function  = \"True\"\n\twhere\n\t\t\t\ta = \"\"\n")

testFunctionWithParameter :: Assertion
testFunctionWithParameter = 
  assertEqual
  "testFunctionWithParameter"
  (functionDefinitionTransformer (FuncDefinition IntType (Ident "my_function") (Parameters []) (Block [VariableDefinition (VariableDefinitionWithoutAssignment IntType (Ident "a"))]) (Literal (IntegerLiteral 0))))
  ("my_function :: Int\nmy_function  = 0\n\twhere\n\t\t\t\ta = 0\n")

testFunctionWithParameters :: Assertion
testFunctionWithParameters =
  assertEqual
  "testFunctionWithParameters"
  (functionDefinitionTransformer (FuncDefinition BooleanType (Ident "my_function") (Parameters [(BooleanType,Ident "a"),(BooleanType,Ident "b")]) (Block [VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (Identifier (Ident "b")))]) (Literal (BooleanLiteral True))))
  ("my_function :: Bool -> Bool -> Bool\nmy_function a b = True\n\twhere\n\t\t\t\ta = b\n")

testFunctionWithFunctionAsParameter :: Assertion
testFunctionWithFunctionAsParameter =
  assertEqual
  "testFunctionWithFunctionAsParameter"
  (functionDefinitionTransformer (FuncDefinition StringType (Ident "my_function") (Parameters [(FunctionType,Ident "func")]) (Block [VariableDefinition (VariableDefinitionWithoutAssignment IntType (Ident "a"))]) (Literal (StringLiteral "passed"))))
  ("my_function :: (Function) -> String\nmy_function func = \"passed\"\n\twhere\n\t\t\t\ta = 0\n")

testFunctionWithWrongFunctionType :: Assertion
testFunctionWithWrongFunctionType =
  assertEqual
  "testFunctionWithWrongFunctionType"
  (functionDefinitionTransformer (FuncDefinition IntType (Ident "my_function") (Parameters [(IntType,Ident "a"),(IntType,Ident "b"),(BooleanType,Ident "c")]) (Block [VariableDefinition (VariableDefinitionWithoutAssignment IntType (Ident "a"))]) (Identifier (Ident "a"))))
  ("my_function :: Int -> Int -> Bool -> Int\nmy_function a b c = a\n\twhere\n\t\t\t\ta = 0\n")

functionTemplateTest :: TestTree
functionTemplateTest =
  testGroup
  "Function Template Test" [
    testCase "testFunctionCharType"
    testFunctionCharType,
    testCase "testFunctionBooleanType"
    testFunctionBooleanType,
    testCase "testFunctionStringType"
    testFunctionStringType,
    testCase "testFunctionWithParameter"
    testFunctionWithParameter,
    testCase "testFunctionWithParameters"
    testFunctionWithParameters,
    testCase "testFunctionWithFunctionAsParameter"
    testFunctionWithFunctionAsParameter,
    testCase "testFunctionWithWrongFunctionType"
    testFunctionWithWrongFunctionType
  ]
