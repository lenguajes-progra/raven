module TypeTest where

import Literal
import Grammar
import Error
import Type
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import Control.Exception (catch)

testVariableDefinitionParserWithAssignment :: Assertion
testVariableDefinitionParserWithAssignment = assertEqual "parseVariableDefinitionWithAssignment" (parse variableDefinitionParser "" "int a = 1;") (Right (VariableDefinitionComplete IntType (Ident "a") (Right (IntegerLiteral 1))))

testVariableDefinitionParserWithoutAssignment :: Assertion
testVariableDefinitionParserWithoutAssignment = assertEqual "parseVariableDefinitionWithoutAssignment" (parse variableDefinitionParser "" "int a;") (Right (VariableDefinitionWithoutAssignment IntType (Ident "a")))

testVariableDefinitionParserAssignTypeError :: Assertion
testVariableDefinitionParserAssignTypeError = assertEqual "parseVariableDefinitionAssignmentError" (parse variableDefinitionParser "" "int a = true;") (Right (VariableDefinitionComplete IntType (Ident "a") (Left (ErrorType AssignType))))

testArrayDefinitionParserWithAssignment :: Assertion
testArrayDefinitionParserWithAssignment = assertEqual "parseArrayDefinitionWithAssignment" (parse arrayDefinitionParser "" "[boolean] arr = [true, false];") (Right (ArrayDefinitionComplete (ArrayType BooleanType) (Ident "arr") (Right (Literals [BooleanLiteral True,BooleanLiteral False]))))

testArrayDefinitionParserWithoutAssignment :: Assertion
testArrayDefinitionParserWithoutAssignment = assertEqual "parseArrayDefinitionWithoutAssignment" (parse arrayDefinitionParser "" "[char] arr;") (Right (ArrayDefinitionWithoutAssignment (ArrayType CharType) (Ident "arr")))

testArrayDefinitionParserAssignTypeError :: Assertion
testArrayDefinitionParserAssignTypeError = assertEqual "parseArrayDefinitionAssignmentError" (parse arrayDefinitionParser "" "[int] arr = [1,2,true];") (Right (ArrayDefinitionComplete (ArrayType IntType) (Ident "arr") (Left (ErrorType AssignType))))

typeTests :: TestTree
typeTests = testGroup "Builder Type Tests" [
    testCase "parseVariableDefinitionWithAssignment" testVariableDefinitionParserWithAssignment,
    testCase "parseVariableDefinitionWithoutAssignment" testVariableDefinitionParserWithoutAssignment,
    testCase "parseVariableDefinitionAssignmentError" testVariableDefinitionParserAssignTypeError,
    testCase "parseArrayDefinitionWithAssignment" testArrayDefinitionParserWithAssignment,
    testCase "parseArrayDefinitionWithoutAssignment" testArrayDefinitionParserWithoutAssignment,
    testCase "parseArrayDefinitionAssignmentError" testArrayDefinitionParserAssignTypeError]