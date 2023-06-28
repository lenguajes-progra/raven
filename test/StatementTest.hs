module StatementTest where

import Literal
import Grammar
import Parsers
import Type
import Statement
import Test.Tasty
import Test.Tasty.HUnit ( testCase, assertEqual, Assertion )
import Text.Parsec
import Control.Exception (catch)

testStatementParseVarDef :: Assertion
testStatementParseVarDef = assertEqual "statementParseVarDef" (parse statementParse "" "int a = 1;") (Right (VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (IntegerLiteral 1))))

testStatementParseArrDef :: Assertion
testStatementParseArrDef = assertEqual "statementParseArrDef" (parse statementParse "" "[boolean] arr = [true, false];") (Right (ArrayDefinition (ArrayDefinitionComplete (ArrayType BooleanType) (Ident "arr") (Literals [BooleanLiteral True,BooleanLiteral False]))))

testStatementParseIf :: Assertion
testStatementParseIf = assertEqual "statementParseIf" (parse statementParse "" "if(a==b) {while (a==b) {int a = 2} end} else {print f} end") (Right (IfStat (IfStatement (NumericExpression (NumericOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [LoopStat (LoopStatement (NumericExpression (NumericOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (IntegerLiteral 2))]))]) (Block [PrintStat (PrintStatement (Identifier (Ident "f")))]))))

testStatementParseLoop :: Assertion
testStatementParseLoop = assertEqual "statementParseLoop" (parse statementParse "" "while (a==b) {int a = 2} end") (Right (LoopStat (LoopStatement (NumericExpression (NumericOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (IntegerLiteral 2))]))))

testStatementParsePrint :: Assertion
testStatementParsePrint = assertEqual "statementParsePrint" (parse statementParse "" "print f") (Right (PrintStat (PrintStatement (Identifier (Ident "f")))))

testStatementParseFunCall :: Assertion
testStatementParseFunCall = assertEqual "statementParseFunCall" (parse statementParse "" "main()") (Right (FuncCallStat (FunctionCall (Ident "main") (ParametersCalled []))))

testStatementParseExp :: Assertion
testStatementParseExp = assertEqual "statementParseExp" (parse statementParse "" "a >= b") (Right (Expression (NumericExpression (NumericOp (Identifier (Ident "a")) GreatEqualThan (Identifier (Ident "b"))))))

testStatementParseEnd :: Assertion
testStatementParseEnd = assertEqual "statementParseEnd" (parse statementParse "" "\n") (Right (End '\n'))

testBlockParse :: Assertion
testBlockParse = assertEqual "blockParse" (parse blockParse "" "print f") (Right (Block [PrintStat (PrintStatement (Identifier (Ident "f")))]))

testBlockParseBB :: Assertion
testBlockParseBB = assertEqual "blockParseBB" (parse blockParseBetweenBrackets "" "{ print f }") (Right (Block [PrintStat (PrintStatement (Identifier (Ident "f")))]))

testIfStmntParser :: Assertion
testIfStmntParser = assertEqual "ifStmntParser" (parse ifStatementParser "" "if(a==b) {while (a==b) {int a = 2} end} else {print f} end") (Right (IfStatement (NumericExpression (NumericOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [LoopStat (LoopStatement (NumericExpression (NumericOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (IntegerLiteral 2))]))]) (Block [PrintStat (PrintStatement (Identifier (Ident "f")))])))

testLoopStmntParser :: Assertion
testLoopStmntParser = assertEqual "loopStmntParser" (parse loopStatementParser "" "while (a==b) {int a = 2} end") (Right (LoopStatement (NumericExpression (NumericOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (IntegerLiteral 2))])))

statementTests :: TestTree
statementTests = testGroup "Builder Statement Tests" [
    testCase "statementParseVarDef" testStatementParseVarDef,
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