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
testStatementParseIf = assertEqual "statementParseIf" (parse statementParse "" "if(a==b) {a = 3} else {a = 4} end") (Right (IfStat (IfStatement (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (Literal (IntegerLiteral 3)))) (VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (Literal (IntegerLiteral 4)))))))

testStatementParseLoop :: Assertion
testStatementParseLoop = assertEqual "statementParseLoop" (parse statementParse "" "for (a; true) {a==b} end") (Right (ForStat (ForStatement (Identifier (Ident "a")) (Literal (BooleanLiteral True)) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))))))

testStatementParsePrint :: Assertion
testStatementParsePrint = assertEqual "statementParsePrint" (parse statementParse "" "print f") (Right (PrintStat (PrintStatement (Identifier (Ident "f")))))

testStatementParseFunCall :: Assertion
testStatementParseFunCall = assertEqual "statementParseFunCall" (parse statementParse "" "main()") (Right (FuncCallStat (FunctionCall (Ident "main") (ParametersCalled []))))

testStatementParseExp :: Assertion
testStatementParseExp = assertEqual "statementParseExp" (parse statementParse "" "a = 3") (Right (VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (Literal (IntegerLiteral 3)))))

testStatementParseEnd :: Assertion
testStatementParseEnd = assertEqual "statementParseEnd" (parse statementParse "" "\n") (Right (End '\n'))

testBlockParse :: Assertion
testBlockParse = assertEqual "blockParse" (parse blockParse "" "print f") (Right (Block [PrintStat (PrintStatement (Identifier (Ident "f")))]))

testBlockParseBB :: Assertion
testBlockParseBB = assertEqual "blockParseBB" (parse expressionParseBetweenBrackets "" "{ a >= 3 }") (Right (BooleanExpression (BooleanOp (Identifier (Ident "a")) GreatEqualThan (Literal (IntegerLiteral 3)))))

testIfStmntParser :: Assertion
testIfStmntParser = assertEqual "ifStmntParser" (parse ifStatementParser "" "if(a==b) {a = 3} else {a = 4} end") (Right (IfStatement (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (Literal (IntegerLiteral 3)))) (VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (Literal (IntegerLiteral 4))))))

testLoopStmntParser :: Assertion
testLoopStmntParser = assertEqual "loopStmntParser" (parse forStatementParser "" "for (a; true) {a==b} end") (Right (ForStatement (Identifier (Ident "a")) (Literal (BooleanLiteral True)) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b"))))))

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