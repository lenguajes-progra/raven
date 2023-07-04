module FunctionTest where

import Control.Exception (catch)
import Function
import Grammar
import Literal
import Parsers
import Statement
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import Type

testFunctionDefinitionParserIntType :: Assertion
testFunctionDefinitionParserIntType = assertEqual "parseFunctionDefIntType" (parse functionDefinitionParser "" "int function() {int a = 2 > 3} return 0 end") (Right (FuncDefinition IntType (Ident "function") (Parameters []) (Block [VariableDefinition (VariableErrorDefinition (ErrorType AssignType))]) (Literal (IntegerLiteral 0))))

testFunctionDefinitionParserCharType :: Assertion
testFunctionDefinitionParserCharType = assertEqual "parseFunctionDefCharType" (parse functionDefinitionParser "" "boolean function(int a, int b) {boolean c = a > b} return c end") (Right (FuncDefinition BooleanType (Ident "function") (Parameters [(IntType,Ident "a"),(IntType,Ident "b")]) (Block [VariableDefinition (VariableDefinitionComplete BooleanType (Ident "c") (BooleanExpression (BooleanOp (Identifier (Ident "a")) GreaterThan (Identifier (Ident "b")))))]) (Identifier (Ident "c"))))

testFunctionDefinitionParserBooleanType :: Assertion
testFunctionDefinitionParserBooleanType = assertEqual "parseFunctionDefBooleanType" (parse functionDefinitionParser "" "boolean my_function() {true = false} return true  end") (Right (FuncDefinition BooleanType (Ident "my_function") (Parameters []) (Block [VariableDefinition (VariableDefinitionWithAssignment (Ident "true") (Literal (BooleanLiteral False)))]) (Literal (BooleanLiteral True))))

testFunctionDefinitionParserStringType :: Assertion
testFunctionDefinitionParserStringType = assertEqual "parseFunctionDefStringType" (parse functionDefinitionParser "" "string my_function() {string a} return \"True\" end") (Right (FuncDefinition StringType (Ident "my_function") (Parameters []) (Block [VariableDefinition (VariableDefinitionWithoutAssignment StringType (Ident "a"))]) (Literal (StringLiteral "True"))))

testFunctionDefinitionWithParameter :: Assertion
testFunctionDefinitionWithParameter = assertEqual "parseFunctionDefWithParameter" (parse functionDefinitionParser "" "int my_function() {int a} return 0 end") (Right (FuncDefinition IntType (Ident "my_function") (Parameters []) (Block [VariableDefinition (VariableDefinitionWithoutAssignment IntType (Ident "a"))]) (Literal (IntegerLiteral 0))))

testFunctionDefinitionWithParameters :: Assertion
testFunctionDefinitionWithParameters = assertEqual "parseFunctionDefWithParameters" (parse functionDefinitionParser "" "boolean my_function(boolean a, boolean b) {a = b} return true end") (Right (FuncDefinition BooleanType (Ident "my_function") (Parameters [(BooleanType,Ident "a"),(BooleanType,Ident "b")]) (Block [VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (Identifier (Ident "b")))]) (Literal (BooleanLiteral True))))

testFunctionDefinitionWithFunctionAsParameter :: Assertion
testFunctionDefinitionWithFunctionAsParameter = assertEqual "parseFunctionDefWithFunctionAsParameter" (parse functionDefinitionParser "" "string my_function(function func) {int a} return \"passed\" end") (Right (FuncDefinition StringType (Ident "my_function") (Parameters [(FunctionType,Ident "func")]) (Block [VariableDefinition (VariableDefinitionWithoutAssignment IntType (Ident "a"))]) (Literal (StringLiteral "passed"))))

testFunctionDefinitionWithWrongFunctionType :: Assertion
testFunctionDefinitionWithWrongFunctionType = assertEqual "parseFunctionDefWithWrongTypeFunction" (parse functionDefinitionParser "" "int my_function(int a, int b, boolean c) {int a} return a end") (Right (FuncDefinition IntType (Ident "my_function") (Parameters [(IntType,Ident "a"),(IntType,Ident "b"),(BooleanType,Ident "c")]) (Block [VariableDefinition (VariableDefinitionWithoutAssignment IntType (Ident "a"))]) (Identifier (Ident "a"))))

functionTests :: TestTree
functionTests =
  testGroup
    "Builder Function Tests"
    [ testCase "parseFunctionDefIntType" testFunctionDefinitionParserIntType,
      testCase "parseFunctionDefCharType" testFunctionDefinitionParserCharType,
      testCase "parseFunctionDefBooleanType" testFunctionDefinitionParserBooleanType,
      testCase "parseFunctionDefStringType" testFunctionDefinitionParserStringType,
      testCase "parseFunctionDefWithParameter" testFunctionDefinitionWithParameter,
      testCase "parseFunctionDefWithParameters" testFunctionDefinitionWithParameters,
      testCase "parseFunctionDefWithFunctionAsParameter" testFunctionDefinitionWithFunctionAsParameter,
      testCase "parseFunctionDefWithWrongTypeFunction" testFunctionDefinitionWithWrongFunctionType
    ]