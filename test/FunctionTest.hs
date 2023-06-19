module FunctionTest where

import Literal
import Grammar
import Parsers
import Type
import Expression
import Function
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import Control.Exception (catch)

testFunctionDefinitionParserIntType :: Assertion
testFunctionDefinitionParserIntType = assertEqual "parseFunctionDefIntType" (parse functionDefinitionParser "" "int function() block 2 > 3  end") (Right (FuncDefinition IntType (Ident "function") (Parameters []) "block" (NumericExpression (NumericOp (Literal (IntegerLiteral 2)) GreaterThan (Literal (IntegerLiteral 3))))))

testFunctionDefinitionParserCharType :: Assertion
testFunctionDefinitionParserCharType = assertEqual "parseFunctionDefCharType" (parse functionDefinitionParser "" "char function() block 10 << 100  end") (Right (FuncDefinition CharType (Ident "function") (Parameters []) "block" (BitExpression (BitOp (Literal (IntegerLiteral 10)) LeftShift (Literal (IntegerLiteral 100))))))

testFunctionDefinitionParserBooleanType :: Assertion
testFunctionDefinitionParserBooleanType = assertEqual "parseFunctionDefBooleanType" (parse functionDefinitionParser "" "boolean my_function() block true != false  end") (Right (FuncDefinition BooleanType (Ident "my_function") (Parameters []) "block" (NumericExpression (NumericOp (Literal (BooleanLiteral True)) NotEqual (Literal (BooleanLiteral False))))))

testFunctionDefinitionParserStringType :: Assertion
testFunctionDefinitionParserStringType = assertEqual "parseFunctionDefStringType" (parse functionDefinitionParser "" "string my_function() block !false  end") (Right (FuncDefinition StringType (Ident "my_function") (Parameters []) "block" (LogicalExpression (LogicNot (Literal (BooleanLiteral False))))))

testFunctionDefinitionWithParameter :: Assertion
testFunctionDefinitionWithParameter = assertEqual "parseFunctionDefWithParameter" (parse functionDefinitionParser "" "string my_function(boolean a) block !a  end") (Right (FuncDefinition StringType (Ident "my_function") (Parameters [(BooleanType,Ident "a")]) "block" (LogicalExpression (LogicNot (Identifier (Ident "a"))))))

testFunctionDefinitionWithParameters :: Assertion
testFunctionDefinitionWithParameters = assertEqual "parseFunctionDefWithParameters" (parse functionDefinitionParser "" "string my_function(boolean a, boolean b) block a == b  end") (Right (FuncDefinition StringType (Ident "my_function") (Parameters [(BooleanType,Ident "a"),(BooleanType,Ident "b")]) "block" (NumericExpression (NumericOp (Identifier (Ident "a")) Equal (Identifier (Ident "b"))))))

testFunctionDefinitionWithArrayAsParameter :: Assertion
testFunctionDefinitionWithArrayAsParameter = assertEqual "parseFunctionDefWithArrayAsParameter" (parse functionDefinitionParser "" "string my_function([boolean] arr) block 5 >= 6  end") (Right (FuncDefinition StringType (Ident "my_function") (Parameters [(ArrayType BooleanType,Ident "arr")]) "block" (NumericExpression (NumericOp (Literal (IntegerLiteral 5)) GreatEqualThan (Literal (IntegerLiteral 6))))))

testFunctionDefinitionWithArraysAsParameters :: Assertion
testFunctionDefinitionWithArraysAsParameters = assertEqual "parseFunctionDefWithArraysAsParameters" (parse functionDefinitionParser "" "int my_function([boolean] arr, [int] arr2, [string] arr3) block 5 <= 6  end") (Right (FuncDefinition IntType (Ident "my_function") (Parameters [(ArrayType BooleanType,Ident "arr"),(ArrayType IntType,Ident "arr2"),(ArrayType StringType,Ident "arr3")]) "block" (NumericExpression (NumericOp (Literal (IntegerLiteral 5)) LessEqualThan (Literal (IntegerLiteral 6))))))

functionTests :: TestTree
functionTests = testGroup "Builder Function Tests" [
    testCase "parseFunctionDefIntType" testFunctionDefinitionParserIntType,
    testCase "parseFunctionDefCharType" testFunctionDefinitionParserCharType,
    testCase "parseFunctionDefBooleanType" testFunctionDefinitionParserBooleanType,
    testCase "parseFunctionDefStringType" testFunctionDefinitionParserStringType,
    testCase "parseFunctionDefWithParameter" testFunctionDefinitionWithParameter,
    testCase "parseFunctionDefWithParameters" testFunctionDefinitionWithParameters,
    testCase "parseFunctionDefWithArrayAsParameter" testFunctionDefinitionWithArrayAsParameter,
    testCase "parseFunctionDefWithArraysAsParameters" testFunctionDefinitionWithArraysAsParameters]