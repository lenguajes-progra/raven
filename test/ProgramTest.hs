module ProgramTest where

import Grammar
import Program
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec

testProgramParserAssingIntegerValue :: Assertion
testProgramParserAssingIntegerValue =
  assertEqual
    "testProgramParserAssingIntegerValue"
    (parse programParser "" "main() `int a = 2; int b = 1` end")
    (Right (Right (Program (FuncDefList []) (Block [VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (Right (IntegerLiteral 2))), VariableDefinition (VariableDefinitionComplete IntType (Ident "b") (Right (IntegerLiteral 1)))]))))

testProgramParserAssingStringValue :: Assertion
testProgramParserAssingStringValue = 
  assertEqual
  "testProgramParserAssingStringValue"
  (parse programParser "" "main() `string a = \"hello\"; string b = \"world\"` end")
  (Right (Right (Program (FuncDefList []) (Block [VariableDefinition (VariableDefinitionComplete StringType (Ident "a") (Right (StringLiteral "hello"))),VariableDefinition (VariableDefinitionComplete StringType (Ident "b") (Right (StringLiteral "world")))]))))

testProgramParserAssingCharValue :: Assertion
testProgramParserAssingCharValue =
  assertEqual
  "testProgramParserAssingCharValue"
  (parse programParser "" "main() `char a = 'a'; char b = 'a'` end")
  (Right (Right (Program (FuncDefList []) (Block [VariableDefinition (VariableDefinitionComplete CharType (Ident "a") (Right (CharacterLiteral 'a'))),VariableDefinition (VariableDefinitionComplete CharType (Ident "b") (Right (CharacterLiteral 'a')))]))))

testProgramParserInvalidAssingIntegerValue :: Assertion
testProgramParserInvalidAssingIntegerValue =
  assertEqual
  "testProgramParserInvalidAssingIntegerValue"
  (parse programParser "" "main() `int a = a; int b = b` end")
  (Right (Left (ErrorType Syntax)))

testProgramParserInvalidAssingStringValue :: Assertion
testProgramParserInvalidAssingStringValue =
  assertEqual
  "testProgramParserInvalidAssingStringValue"
  (parse programParser "" "main() `string a = 1; string b = 2` end")
  (Right (Right (Program (FuncDefList []) (Block [VariableDefinition (VariableDefinitionComplete StringType (Ident "a") (Left (ErrorType AssignType))),VariableDefinition (VariableDefinitionComplete StringType (Ident "b") (Left (ErrorType AssignType)))]))))

testProgramParserInvalidAssingCharValue :: Assertion
testProgramParserInvalidAssingCharValue =
  assertEqual
  "testProgramParserInvalidAssingCharValue"
  (parse programParser "" "main() `char a = 1; char b = 2` end")
  (Right (Right (Program (FuncDefList []) (Block [VariableDefinition (VariableDefinitionComplete CharType (Ident "a") (Left (ErrorType AssignType))),VariableDefinition (VariableDefinitionComplete CharType (Ident "b") (Left (ErrorType AssignType)))]))))

programTest :: TestTree
programTest =
  testGroup
    "Builder Function Tests"
    [ testCase "testProgramParserAssingIntegerValue" testProgramParserAssingIntegerValue,
      testCase "testProgramParserAssingStringValue" testProgramParserAssingStringValue,
      testCase "testProgramParserAssingCharValue" testProgramParserAssingCharValue,
      testCase "testProgramParserInvalidAssingIntegerValue" testProgramParserInvalidAssingIntegerValue,
      testCase "testProgramParserInvalidAssingStringValue" testProgramParserInvalidAssingStringValue,
      testCase "testProgramParserInvalidAssingCharValue" testProgramParserInvalidAssingCharValue
    ]
