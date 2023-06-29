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
testFunctionDefinitionParserIntType = assertEqual "parseFunctionDefIntType" (parse functionDefinitionParser "" "int function() {2 > 3} return 0 end") (Right (FuncDefinition IntType (Ident "function") (Parameters []) (Block [Expression (BooleanExpression (BooleanOp (Literal (IntegerLiteral 2)) GreaterThan (Literal (IntegerLiteral 3))))]) (Literal (IntegerLiteral 0))))

testFunctionDefinitionParserCharType :: Assertion
testFunctionDefinitionParserCharType = assertEqual "parseFunctionDefCharType" (parse functionDefinitionParser "" "char function() {10 << 100} return 'a'  end") (Right (FuncDefinition CharType (Ident "function") (Parameters []) (Block [Expression (BitExpression (BitOp (Literal (IntegerLiteral 10)) LeftShift (Literal (IntegerLiteral 100))))]) (Literal (CharacterLiteral 'a'))))

testFunctionDefinitionParserBooleanType :: Assertion
testFunctionDefinitionParserBooleanType = assertEqual "parseFunctionDefBooleanType" (parse functionDefinitionParser "" "boolean my_function() {true != false} return true  end") (Right (FuncDefinition BooleanType (Ident "my_function") (Parameters []) (Block [Expression (BooleanExpression (BooleanOp (Literal (BooleanLiteral True)) NotEqual (Literal (BooleanLiteral False))))]) (Literal (BooleanLiteral True))))

testFunctionDefinitionParserStringType :: Assertion
testFunctionDefinitionParserStringType = assertEqual "parseFunctionDefStringType" (parse functionDefinitionParser "" "string my_function() {!false} return \"True\"  end") (Right (FuncDefinition StringType (Ident "my_function") (Parameters []) (Block [Expression (LogicalExpression (LogicNot (Literal (BooleanLiteral False))))]) (Literal (StringLiteral "True"))))

testFunctionDefinitionWithParameter :: Assertion
testFunctionDefinitionWithParameter = assertEqual "parseFunctionDefWithParameter" (parse functionDefinitionParser "" "int my_function(boolean a) {~a} return 0 end") (Right (FuncDefinition IntType (Ident "my_function") (Parameters [(BooleanType, Ident "a")]) (Block [Expression (BitExpression (BitNot (Identifier (Ident "a"))))]) (Literal (IntegerLiteral 0))))

testFunctionDefinitionWithParameters :: Assertion
testFunctionDefinitionWithParameters = assertEqual "parseFunctionDefWithParameters" (parse functionDefinitionParser "" "boolean my_function(boolean a, boolean b) {a == b} return true end") (Right (FuncDefinition BooleanType (Ident "my_function") (Parameters [(BooleanType, Ident "a"), (BooleanType, Ident "b")]) (Block [Expression (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b"))))]) (Literal (BooleanLiteral True))))

testFunctionDefinitionWithFunctionAsParameter :: Assertion
testFunctionDefinitionWithFunctionAsParameter = assertEqual "parseFunctionDefWithFunctionAsParameter" (parse functionDefinitionParser "" "string my_function(function func) {5 >= 6} return \"passed\"  end") (Right (FuncDefinition StringType (Ident "my_function") (Parameters [(FunctionType, Ident "func")]) (Block [Expression (BooleanExpression (BooleanOp (Literal (IntegerLiteral 5)) GreatEqualThan (Literal (IntegerLiteral 6))))]) (Literal (StringLiteral "passed"))))

testFunctionDefinitionWithWrongFunctionType :: Assertion
testFunctionDefinitionWithWrongFunctionType = assertEqual "parseFunctionDefWithWrongTypeFunction" (parse functionDefinitionParser "" "int my_function(int a, int b, boolean c) {a <= b} return true end") (Right (FuncDefinitionError (ErrorType TypeFunction)))

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