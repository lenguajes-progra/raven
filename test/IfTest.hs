module IfTest where

import Literal
import Grammar
import Parsers
import Type
import Expression
import Statement
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import Control.Exception (catch)

testIfStatement :: Assertion
testIfStatement = assertEqual "parseIfStatement" (parse ifStatementParser "" "if(a==b) {while (a==b) {int a = 2} end} else {print f} end") (Right (IfStatement (NumericExpression (NumericOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [LoopStat (LoopStatement (NumericExpression (NumericOp (Identifier (Ident "a")) Equal (Identifier (Ident "b")))) (Block [VariableDefinition (VariableDefinitionComplete IntType (Ident "a") (Right (IntegerLiteral 2)))]))]) (Block [PrintStat (PrintStatement (Identifier (Ident "f")))])))

ifTests :: TestTree
ifTests = testGroup "Builder If Tests" [
    testCase "parseIfStatement" testIfStatement]