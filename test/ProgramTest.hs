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



programTest :: TestTree
programTest =
  testGroup
    "Builder Function Tests"
    [ testCase "testProgramParserAssingIntegerValue" testProgramParserAssingIntegerValue,
      testCase "testProgramParserAssingStringValue" testProgramParserAssingStringValue,
      testCase "testProgramParserAssingCharValue" testProgramParserAssingCharValue
    ]
