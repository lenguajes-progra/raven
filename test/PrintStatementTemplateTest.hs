module PrintStatementTemplateTest where

import PrintStatementTemplate
import Control.Exception (catch)
import DataTransformer
import Grammar
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import GHC.IO.Exception (assertError)

testPrintStatementTransformerString :: Assertion
testPrintStatementTransformerString =
  assertEqual
  "testPrintStatementTransformerString"
  (printStatementTransformer (PrintStatement (Literal (StringLiteral "Hello World"))) []) ("main :: IO()\nmain = print(\"Hello World\")\n")

testPrintStatementTransformerNumber :: Assertion
testPrintStatementTransformerNumber =
  assertEqual
  "testPrintStatementTransformerNumber"
  (printStatementTransformer (PrintStatement (Literal (IntegerLiteral 1))) []) ("main :: IO()\nmain = print(1)\n")

testPrintStatementTransformerChar :: Assertion
testPrintStatementTransformerChar =
  assertEqual
  "testPrintStatementTransformerChar"
  (printStatementTransformer (PrintStatement (Literal (CharacterLiteral 'a'))) []) ("main :: IO()\nmain = print('a')\n")

printStatementTemplateTest :: TestTree
printStatementTemplateTest =
  testGroup
  "PrintStatement Template Test" [
    testCase "testPrintStatementTransformerString"
    testPrintStatementTransformerString,
    testCase "testPrintStatementTransformerNumber"
    testPrintStatementTransformerNumber,
    testCase "testPrintStatementTransformerChar"
    testPrintStatementTransformerChar
  ]
  