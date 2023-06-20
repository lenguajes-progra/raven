module PrintStatementTest where

import Control.Exception (catch)
import Grammar
import PrintStatement
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec

testStringWithSpacePrintStatementParser :: Assertion
testStringWithSpacePrintStatementParser =
  assertEqual
    "testStringWithSpacePrintStatementParser"
    (parse printStatementParser "" "print \"Hello World\"")
    (Right (PrintStatement (Literal (StringLiteral "Hello World"))))

testSingleStringPrintStatementParser :: Assertion
testSingleStringPrintStatementParser =
  assertEqual
    "testSingleStringPrintStatementParser"
    (parse printStatementParser "" "print \"Hello\"")
    (Right (PrintStatement (Literal (StringLiteral "Hello"))))

testSpecialCharacterStringPrintStatementParser :: Assertion
testSpecialCharacterStringPrintStatementParser =
  assertEqual
    "testSpecialCharacterStringPrintStatementParser"
    (parse printStatementParser "" "print \"Hello$\"")
    (Right (PrintStatement (Literal (StringLiteral "Hello$"))))

testSpecialCharacterStringSpacePrintStatementParser :: Assertion
testSpecialCharacterStringSpacePrintStatementParser =
  assertEqual
    "testSpecialCharacterStringSpacePrintStatementParser"
    (parse printStatementParser "" "print \"Hello$\"")
    (Right (PrintStatement (Literal (StringLiteral "Hello $"))))

printStatementTest :: TestTree
printStatementTest =
  testGroup
    "Builder Function Tests"
    [ testCase "testStringWithSpacePrintStatementParser" testStringWithSpacePrintStatementParser
    ]