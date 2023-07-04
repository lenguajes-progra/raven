module StatementTemplateTest where

import StatementTransformer
import Grammar
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

testStatementTransformerVarDef :: Assertion
testStatementTransformerVarDef = assertEqual "statementTransformerVarDef" (statementTransformer (VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (Literal (IntegerLiteral 1)))) []) "a :: Int\na = 1\n"

testStatementTransformerArrDef :: Assertion
testStatementTransformerArrDef = assertEqual "statementTransformerArrDef" (statementTransformer (ArrayDefinition (ArrayDefinitionComplete (ArrayType BooleanType) (Ident "arr") (Literals [BooleanLiteral True, BooleanLiteral False]))) []) "arr :: [Bool]\narr = [True, False]\n"

testStatementTransformerPrintStat :: Assertion
testStatementTransformerPrintStat = assertEqual "statementTransformerPrintStat" (statementTransformer (PrintStat (PrintStatement (Identifier (Ident "f")))) []) "main :: IO()\nmain = print(f)\n"

testStatementTransformerFunCallStat :: Assertion
testStatementTransformerFunCallStat = assertEqual "statementTransformerFunCallStat" (statementTransformer (FuncCallStat (FunctionCall (Ident "main") (ParametersCalled []))) []) "(main)"

testStatementTransformerIfStat :: Assertion
testStatementTransformerIfStat = assertEqual "statementTransformerIfStat" (statementTransformer (IfStat (IfStatement (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (Literal (IntegerLiteral 3)))) (VariableDefinition (VariableDefinitionWithAssignment (Ident "a") (Literal (IntegerLiteral 4)))))) []) "if a==b then 3 else 4"

testStatementTransformerForStat :: Assertion
testStatementTransformerForStat = assertEqual "statementTransformerForStat" (statementTransformer (ForStat (ForStatement (Identifier (Ident "a")) (Literal (BooleanLiteral True)) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))))) []) "(for a (\\x -> True) (\\x -> x==b))"

statementTransformerTests :: TestTree
statementTransformerTests =
  testGroup
    "Builder statementTransformer Tests"
    [ 
      testCase "statementTransformerVarDef" testStatementTransformerVarDef,
      testCase "statementTransformerArrDef" testStatementTransformerArrDef,
      testCase "statementTransformerPrintStat" testStatementTransformerPrintStat,
      testCase "statementTransformerFunCallStat" testStatementTransformerFunCallStat,
      testCase "statementTransformerIfStat" testStatementTransformerIfStat,
      testCase "statementTransformerForStat" testStatementTransformerForStat
    ]