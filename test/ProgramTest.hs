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
    (parse programParser "" "boolean hello2() {boolean b = true} return true end")
    (Right (Right (Program (FuncDefList [FuncDefinition BooleanType (Ident "hello2") (Parameters []) (Block [VariableDefinition (VariableDefinitionComplete BooleanType (Ident "b") (Literal (BooleanLiteral True)))]) (Literal (BooleanLiteral True))]))))

testProgramParserAssingStringValue :: Assertion
testProgramParserAssingStringValue =
  assertEqual
  "testProgramParserAssingStringValue"
  (parse programParser "" "boolean func() {string a = \"hello\"; string b = \"world\"} return true end")
  (Right (Right (Program (FuncDefList [FuncDefinition BooleanType (Ident "func") (Parameters []) (Block [VariableDefinition (VariableDefinitionComplete StringType (Ident "a") (Literal (StringLiteral "hello"))),VariableDefinition (VariableDefinitionComplete StringType (Ident "b") (Literal (StringLiteral "world")))]) (Literal (BooleanLiteral True))]))))

testProgramParserAssingCharValue :: Assertion
testProgramParserAssingCharValue =
  assertEqual
  "testProgramParserAssingCharValue"
  (parse programParser "" "boolean func() {char a = 'a'; char b = 'a'} return true end")
  (Right (Right (Program (FuncDefList [FuncDefinition BooleanType (Ident "func") (Parameters []) (Block [VariableDefinition (VariableDefinitionComplete CharType (Ident "a") (Literal (CharacterLiteral 'a'))),VariableDefinition (VariableDefinitionComplete CharType (Ident "b") (Literal (CharacterLiteral 'a')))]) (Literal (BooleanLiteral True))]))))

testProgramParserInvalidAssingIntegerValue :: Assertion
testProgramParserInvalidAssingIntegerValue =
  assertEqual
    "testProgramParserInvalidAssingIntegerValue"
    (parse programParser "" "boolean func() {a int a = a; int b = b} return true end ")
    (Right (Left (ErrorType Syntax)))

testProgramParserInvalidAssingStringValue :: Assertion
testProgramParserInvalidAssingStringValue =
  assertEqual
  "testProgramParserInvalidAssingStringValue"
  (parse programParser "" "boolean func() {a string a = a; string b = b} return true end ")
  (Right (Left (ErrorType Syntax)))

testProgramParserInvalidAssingCharValue :: Assertion
testProgramParserInvalidAssingCharValue =
  assertEqual
  "testProgramParserInvalidAssingCharValue"
  (parse programParser "" "boolean func() {a string a = a; string b = b} return true end ")
  (Right (Left (ErrorType Syntax)))

testIfProgramParser :: Assertion
testIfProgramParser =
  assertEqual
    "testIfProgramParser"
  (parse programParser "" "boolean func() {if(a==b) {a = 3} else {a = 4} end} return true end")
  (Right (Right (Program (FuncDefList [FuncDefinition BooleanType (Ident "func") (Parameters []) (Block [IfStat (IfStatement (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (Literal (IntegerLiteral 3)))) (VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (Literal (IntegerLiteral 4)))))]) (Literal (BooleanLiteral True))]))))

testWhileProgramParser :: Assertion
testWhileProgramParser =
  assertEqual
  "testWhileProgramParser"
  (parse programParser "" "boolean func() {for (a; true) {a==b} end} return true end")
  (Right (Right (Program (FuncDefList [FuncDefinition BooleanType (Ident "func") (Parameters []) (Block [ForStat (ForStatement (Identifier (Ident "a")) (Literal (BooleanLiteral True)) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))))]) (Literal (BooleanLiteral True))]))))

testEqualExpressionProgramParser :: Assertion
testEqualExpressionProgramParser =
  assertEqual
    "testEqualExpressionProgramParser"
    (parse programParser "" "boolean func() {a = a==b} return true end")
    (Right (Right (Program (FuncDefList [FuncDefinition BooleanType (Ident "func") (Parameters []) (Block [VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))))]) (Literal (BooleanLiteral True))]))))

testGreatEqualThanExpressionProgramParser :: Assertion
testGreatEqualThanExpressionProgramParser =
  assertEqual
    "testGreatEqualThanExpressionProgramParser"
    (parse programParser "" "boolean func() {a = a>=b} return true end")
    (Right (Right (Program (FuncDefList [FuncDefinition BooleanType (Ident "func") (Parameters []) (Block [VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (BooleanExpression (BooleanOp (Identifier (Ident "a")) GreatEqualThan (Identifier (Ident "b")))))]) (Literal (BooleanLiteral True))]))))

testLessEqualThanExpressionProgramParser :: Assertion
testLessEqualThanExpressionProgramParser =
  assertEqual
    "testLessEqualThanExpressionProgramParser"
    (parse programParser "" "boolean func() {a = a<=b} return true end")
    (Right (Right (Program (FuncDefList [FuncDefinition BooleanType (Ident "func") (Parameters []) (Block [VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (BooleanExpression (BooleanOp (Identifier (Ident "a")) LessEqualThan (Identifier (Ident "b")))))]) (Literal (BooleanLiteral True))]))))

testNotEqualxpressionProgramParser :: Assertion
testNotEqualxpressionProgramParser =
  assertEqual
    "testNotEquaExpressionProgramParser"
    (parse programParser "" "boolean func() {a = a!=b} return true end")
    (Right (Right (Program (FuncDefList [FuncDefinition BooleanType (Ident "func") (Parameters []) (Block [VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (BooleanExpression (BooleanOp (Identifier (Ident "a")) NotEqual (Identifier (Ident "b")))))]) (Literal (BooleanLiteral True))]))))

testGreaterThanExpressionProgramParser :: Assertion
testGreaterThanExpressionProgramParser =
  assertEqual
    "testGreaterThanExpressionProgramParser"
    (parse programParser "" "boolean func() {a = a>b} return true end")
    (Right (Right (Program (FuncDefList [FuncDefinition BooleanType (Ident "func") (Parameters []) (Block [VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (BooleanExpression (BooleanOp (Identifier (Ident "a")) GreaterThan (Identifier (Ident "b")))))]) (Literal (BooleanLiteral True))]))))

testLessThanExpressionProgramParser :: Assertion
testLessThanExpressionProgramParser =
  assertEqual
    "testLessThanExpressionProgramParser"
    (parse programParser "" "boolean func() {a = a<b} return true end")
    (Right (Right (Program (FuncDefList [FuncDefinition BooleanType (Ident "func") (Parameters []) (Block [VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (BooleanExpression (BooleanOp (Identifier (Ident "a")) LessThan (Identifier (Ident "b")))))]) (Literal (BooleanLiteral True))]))))

programTest :: TestTree
programTest =
  testGroup
    "Builder Program Tests"
    [ testCase "testProgramParserAssingIntegerValue" testProgramParserAssingIntegerValue,
      testCase "testProgramParserAssingStringValue" testProgramParserAssingStringValue,
      testCase "testProgramParserAssingCharValue" testProgramParserAssingCharValue,
      testCase "testProgramParserInvalidAssingIntegerValue" testProgramParserInvalidAssingIntegerValue,
      testCase "testProgramParserInvalidAssingStringValue" testProgramParserInvalidAssingStringValue,
      testCase "testProgramParserInvalidAssingCharValue" testProgramParserInvalidAssingCharValue,
      testCase "testIfProgramParser" testIfProgramParser,
      testCase "testWhileProgramParser" testWhileProgramParser
    ]
