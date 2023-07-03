module StatementTest where

import Grammar
import Statement
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Text.Parsec

testStatementParseVarDef :: Assertion
testStatementParseVarDef = assertEqual "statementParseVarDef" (parse statementParse "" "int a = 1;") (Right (VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (Literal (IntegerLiteral 1)))))

testStatementParseArrDef :: Assertion
testStatementParseArrDef = assertEqual "statementParseArrDef" (parse statementParse "" "[boolean] arr = [true, false];") (Right (ArrayDefinition (ArrayDefinitionComplete (ArrayType BooleanType) (Ident "arr") (Literals [BooleanLiteral True, BooleanLiteral False]))))

testStatementParseIf :: Assertion
testStatementParseIf = assertEqual "statementParseIf" (parse statementParse "" "if(a==b) {for (a;a==b) {int a = 2} end} else {print f} end") (Right (IfStat (IfStatement (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [ForStat (ForStatement (Identifier (Ident "a")) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (Literal (IntegerLiteral 2)))]))]) (Block [PrintStat (PrintStatement (Identifier (Ident "f")))]))))

testStatementParseLoop :: Assertion
testStatementParseLoop = assertEqual "statementParseLoop" (parse statementParse "" "for (a;a==b) {int a = 2} end") (Right (ForStat (ForStatement (Identifier (Ident "a")) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (Literal (IntegerLiteral 2)))]))))

testStatementParsePrint :: Assertion
testStatementParsePrint = assertEqual "statementParsePrint" (parse statementParse "" "print f") (Right (PrintStat (PrintStatement (Identifier (Ident "f")))))

testStatementParseFunCall :: Assertion
testStatementParseFunCall = assertEqual "statementParseFunCall" (parse statementParse "" "main()") (Right (FuncCallStat (FunctionCall (Ident "main") (ParametersCalled []))))

testStatementParseExp :: Assertion
testStatementParseExp = assertEqual "statementParseExp" (parse statementParse "" "a >= b") (Right (Expression (BooleanExpression (BooleanOp (Identifier (Ident "a")) GreatEqualThan (Identifier (Ident "b"))))))

testStatementParseEnd :: Assertion
testStatementParseEnd = assertEqual "statementParseEnd" (parse statementParse "" "\n") (Right (End '\n'))

testBlockParse :: Assertion
testBlockParse = assertEqual "blockParse" (parse blockParse "" "print f") (Right (Block [PrintStat (PrintStatement (Identifier (Ident "f")))]))

testBlockParseBB :: Assertion
testBlockParseBB = assertEqual "blockParseBB" (parse blockParseBetweenBrackets "" "{ print f }") (Right (Block [PrintStat (PrintStatement (Identifier (Ident "f")))]))

testIfStmntParser :: Assertion
testIfStmntParser = assertEqual "ifStmntParser" (parse ifStatementParser "" "if(a==b) {for (a;a==b) {int a = 2} end} else {print f} end") (Right (IfStatement (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [ForStat (ForStatement (Identifier (Ident "a")) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (Literal (IntegerLiteral 2)))]))]) (Block [PrintStat (PrintStatement (Identifier (Ident "f")))])))

testLoopStmntParser :: Assertion
testLoopStmntParser = assertEqual "loopStmntParser" (parse forStatementParser "" "for (a;a==b) {int a = 2} end") (Right (ForStatement (Identifier (Ident "a")) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (Literal (IntegerLiteral 2)))])))

statementTests :: TestTree
statementTests =
  testGroup
    "Builder Statement Tests"
    [ testCase "statementParseVarDef" testStatementParseVarDef,
      testCase "statementParseArrDef" testStatementParseArrDef,
      testCase "statementParseIf" testStatementParseIf,
      testCase "statementParseLoop" testStatementParseLoop,
      testCase "statementParsePrint" testStatementParsePrint,
      testCase "statementParseFunCall" testStatementParseFunCall,
      testCase "statementParseExp" testStatementParseExp,
      testCase "statementParseEnd" testStatementParseEnd,
      testCase "blockParse" testBlockParse,
      testCase "blockParseBB" testBlockParseBB,
      testCase "testIfStmnt" testIfStmntParser,
      testCase "loopStmntParser" testLoopStmntParser
    ]