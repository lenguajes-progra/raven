module FunctionTest where

import Literal
import Grammar
import Parsers
import Type
import Statement
import Function
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import Control.Exception (catch)

testFunctionDefinitionParserIntType :: Assertion
testFunctionDefinitionParserIntType = assertEqual "parseFunctionDefIntType" (parse functionDefinitionParser "" "int function() {2 > 3} return 0 end") (Right (FuncDefinition IntType (Ident "function") (Parameters []) (Block [Expression (NumericExpression (NumericOp (Literal (IntegerLiteral 2)) GreaterThan (Literal (IntegerLiteral 3))))]) (Right (Literal (IntegerLiteral 0)))))

testFunctionDefinitionParserCharType :: Assertion
testFunctionDefinitionParserCharType = assertEqual "parseFunctionDefCharType" (parse functionDefinitionParser "" "char function() {10 << 100} return 'a'  end") (Right (FuncDefinition CharType (Ident "function") (Parameters []) (Block [Expression (BitExpression (BitOp (Literal (IntegerLiteral 10)) LeftShift (Literal (IntegerLiteral 100))))]) (Right (Literal (CharacterLiteral 'a')))))

testFunctionDefinitionParserBooleanType :: Assertion
testFunctionDefinitionParserBooleanType = assertEqual "parseFunctionDefBooleanType" (parse functionDefinitionParser "" "boolean my_function() {true != false} return true  end") (Right (FuncDefinition BooleanType (Ident "my_function") (Parameters []) (Block [Expression (NumericExpression (NumericOp (Literal (BooleanLiteral True)) NotEqual (Literal (BooleanLiteral False))))]) (Right (Literal (BooleanLiteral True)))))

testFunctionDefinitionParserStringType :: Assertion
testFunctionDefinitionParserStringType = assertEqual "parseFunctionDefStringType" (parse functionDefinitionParser "" "string my_function() {!false} return \"True\"  end") (Right (FuncDefinition StringType (Ident "my_function") (Parameters []) (Block [Expression (LogicalExpression (LogicNot (Literal (BooleanLiteral False))))]) (Right (Literal (StringLiteral "True")))))

testFunctionDefinitionWithParameter :: Assertion
testFunctionDefinitionWithParameter = assertEqual "parseFunctionDefWithParameter" (parse functionDefinitionParser "" "int my_function(boolean a) {~a} return 0 end") (Right (FuncDefinition IntType (Ident "my_function") (Parameters [(BooleanType,Ident "a")]) (Block [Expression (BitExpression (BitNot (Identifier (Ident "a"))))]) (Right (Literal (IntegerLiteral 0)))))

testFunctionDefinitionWithParameters :: Assertion
testFunctionDefinitionWithParameters = assertEqual "parseFunctionDefWithParameters" (parse functionDefinitionParser "" "boolean my_function(boolean a, boolean b) {a == b} return true end") (Right (FuncDefinition BooleanType (Ident "my_function") (Parameters [(BooleanType,Ident "a"),(BooleanType,Ident "b")]) (Block [Expression (NumericExpression (NumericOp (Identifier (Ident "a")) Equal (Identifier (Ident "b"))))]) (Right (Literal (BooleanLiteral True)))))

testFunctionDefinitionWithFunctionAsParameter :: Assertion
testFunctionDefinitionWithFunctionAsParameter = assertEqual "parseFunctionDefWithFunctionAsParameter" (parse functionDefinitionParser "" "string my_function(function func) {5 >= 6} return \"passed\"  end") (Right (FuncDefinition StringType (Ident "my_function") (Parameters [(FunctionType,Ident "func")]) (Block [Expression (NumericExpression (NumericOp (Literal (IntegerLiteral 5)) GreatEqualThan (Literal (IntegerLiteral 6))))]) (Right (Literal (StringLiteral "passed")))))

testFunctionDefinitionWithWrongFunctionType :: Assertion
testFunctionDefinitionWithWrongFunctionType = assertEqual "parseFunctionDefWithWrongTypeFunction" (parse functionDefinitionParser "" "int my_function(int a, int b, boolean c) {a <= b} return c end") (Right (FuncDefinition IntType (Ident "my_function") (Parameters [(IntType,Ident "a"),(IntType,Ident "b"),(BooleanType,Ident "c")]) (Block [Expression (NumericExpression (NumericOp (Identifier (Ident "a")) LessEqualThan (Identifier (Ident "b"))))]) (Left (ErrorType TypeFunction))))

functionTests :: TestTree
functionTests = testGroup "Builder Function Tests" [
    testCase "parseFunctionDefIntType" testFunctionDefinitionParserIntType,
    testCase "parseFunctionDefCharType" testFunctionDefinitionParserCharType,
    testCase "parseFunctionDefBooleanType" testFunctionDefinitionParserBooleanType,
    testCase "parseFunctionDefStringType" testFunctionDefinitionParserStringType,
    testCase "parseFunctionDefWithParameter" testFunctionDefinitionWithParameter,
    testCase "parseFunctionDefWithParameters" testFunctionDefinitionWithParameters,
    testCase "parseFunctionDefWithFunctionAsParameter" testFunctionDefinitionWithFunctionAsParameter,
    testCase "parseFunctionDefWithWrongTypeFunction" testFunctionDefinitionWithWrongFunctionType]