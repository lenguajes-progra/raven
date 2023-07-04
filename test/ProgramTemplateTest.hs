module ProgramTemplateTest where

import ProgramTransformer
import Grammar
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

testProgramTransformer :: Assertion
testProgramTransformer = assertEqual "programTransformer" (programTransformer (Program (FuncDefList [FuncDefinition IntType (Ident "hello") (Parameters [(IntType,Ident "a"),(IntType,Ident "b")]) (Block [VariableDefinition (VariableDefinitionComplete IntType (Ident "ab") (Literal (IntegerLiteral 2))),VariableDefinition (VariableDefinitionWithoutAssignment IntType (Ident "result")),ForStat (ForStatement (Identifier (Ident "a")) (Literal (BooleanLiteral True)) (BooleanExpression (BooleanOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))))]) (Identifier (Ident "ab")),FuncDefinition BooleanType (Ident "hello2") (Parameters []) (Block [VariableDefinition (VariableDefinitionComplete BooleanType (Ident "b") (Literal (BooleanLiteral True)))]) (Literal (BooleanLiteral True))]))) "module Output where\n\nimport Data.Bits\n\nhello :: Int -> Int -> Int\nhello a b = ab\n\twhere\nab :: Int\nab = 2\n\nresult = (for a (\\x -> True) (\\x -> x==b))\nhello2 :: Bool\nhello2  = True\n\twhere\nb :: Bool\nb = True\n\n"

programTransformerTests :: TestTree
programTransformerTests =
  testGroup
    "Builder programTransformer Tests"
    [ 
      testCase "programTransformer" testProgramTransformer
    ]