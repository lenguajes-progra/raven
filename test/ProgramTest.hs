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
    (Right (Right (Program (FuncDefList []) (Block [VariableDefinition (VariableDefinitionComplete StringType (Ident "a") (Right (StringLiteral "hello"))), VariableDefinition (VariableDefinitionComplete StringType (Ident "b") (Right (StringLiteral "world")))]))))

testProgramParserAssingCharValue :: Assertion
testProgramParserAssingCharValue =
  assertEqual
    "testProgramParserAssingCharValue"
    (parse programParser "" "main() `char a = 'a'; char b = 'a'` end")
    (Right (Right (Program (FuncDefList []) (Block [VariableDefinition (VariableDefinitionComplete CharType (Ident "a") (Right (CharacterLiteral 'a'))), VariableDefinition (VariableDefinitionComplete CharType (Ident "b") (Right (CharacterLiteral 'a')))]))))

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
    (Right (Right (Program (FuncDefList []) (Block [VariableDefinition (VariableDefinitionComplete StringType (Ident "a") (Left (ErrorType AssignType))), VariableDefinition (VariableDefinitionComplete StringType (Ident "b") (Left (ErrorType AssignType)))]))))

testProgramParserInvalidAssingCharValue :: Assertion
testProgramParserInvalidAssingCharValue =
  assertEqual
    "testProgramParserInvalidAssingCharValue"
    (parse programParser "" "main() `char a = 1; char b = 2` end")
    (Right (Right (Program (FuncDefList []) (Block [VariableDefinition (VariableDefinitionComplete CharType (Ident "a") (Left (ErrorType AssignType))), VariableDefinition (VariableDefinitionComplete CharType (Ident "b") (Left (ErrorType AssignType)))]))))

testIfProgramParser :: Assertion
testIfProgramParser =
  assertEqual
    "testIfProgramParser"
    (parse programParser "" "main() `if(a==b) {print f} else {print c} end` end")
    (Right (Right (Program (FuncDefList []) (Block [IfStat (IfStatement (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [PrintStat (PrintStatement (Identifier (Ident "f")))]) (Block [PrintStat (PrintStatement (Identifier (Ident "c")))]))]))))

testWhileProgramParser :: Assertion
testWhileProgramParser =
  assertEqual
    "testWhileProgramParser"
    (parse programParser "" "main() `while (a==b) {int a = 2} end` end")
    (Right (Right (Program (FuncDefList []) (Block [LoopStat (LoopStatement (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (Right (IntegerLiteral 2)))]))]))))

testEqualExpressionProgramParser :: Assertion
testEqualExpressionProgramParser =
  assertEqual
    "testEqualExpressionProgramParser"
    (parse programParser "" "main() `a==b` end")
    (Right (Right (Program (FuncDefList []) (Block [Expression (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b"))))]))))

testGreatEqualThanExpressionProgramParser :: Assertion
testGreatEqualThanExpressionProgramParser =
  assertEqual
    "testGreatEqualThanExpressionProgramParser"
    (parse programParser "" "main() `a>=b` end")
    (Right (Right (Program (FuncDefList []) (Block [Expression (BooleanExpression (BooleanOp (Identifier (Ident "a")) GreatEqualThan (Identifier (Ident "b"))))]))))

testLessEqualThanExpressionProgramParser :: Assertion
testLessEqualThanExpressionProgramParser =
  assertEqual
    "testLessEqualThanExpressionProgramParser"
    (parse programParser "" "main() `a<=b` end")
    (Right (Right (Program (FuncDefList []) (Block [Expression (BooleanExpression (BooleanOp (Identifier (Ident "a")) LessEqualThan (Identifier (Ident "b"))))]))))

testNotEqualxpressionProgramParser :: Assertion
testNotEqualxpressionProgramParser =
  assertEqual
    "testNotEquaExpressionProgramParser"
    (parse programParser "" "main() `a!=b` end")
    (Right (Right (Program (FuncDefList []) (Block [Expression (BooleanExpression (BooleanOp (Identifier (Ident "a")) NotEqual (Identifier (Ident "b"))))]))))

testGreaterThanExpressionProgramParser :: Assertion
testGreaterThanExpressionProgramParser =
  assertEqual
    "testGreaterThanExpressionProgramParser"
    (parse programParser "" "main() `a>b` end")
    (Right (Right (Program (FuncDefList []) (Block [Expression (BooleanExpression (BooleanOp (Identifier (Ident "a")) GreaterThan (Identifier (Ident "b"))))]))))

testLessThanExpressionProgramParser :: Assertion
testLessThanExpressionProgramParser =
  assertEqual
    "testLessThanExpressionProgramParser"
    (parse programParser "" "main() `a<b` end")
    (Right (Right (Program (FuncDefList []) (Block [Expression (BooleanExpression (BooleanOp (Identifier (Ident "a")) LessThan (Identifier (Ident "b"))))]))))

programTest :: TestTree
programTest =
  testGroup
    "Builder Function Tests"
    [ testCase "testProgramParserAssingIntegerValue" testProgramParserAssingIntegerValue,
      testCase "testProgramParserAssingStringValue" testProgramParserAssingStringValue,
      testCase "testProgramParserAssingCharValue" testProgramParserAssingCharValue,
      testCase "testProgramParserInvalidAssingIntegerValue" testProgramParserInvalidAssingIntegerValue,
      testCase "testProgramParserInvalidAssingStringValue" testProgramParserInvalidAssingStringValue,
      testCase "testProgramParserInvalidAssingCharValue" testProgramParserInvalidAssingCharValue,
      testCase "testIfProgramParser" testIfProgramParser,
      testCase "testWhileProgramParser" testWhileProgramParser
    ]
