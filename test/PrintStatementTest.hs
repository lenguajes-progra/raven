module PrintStatementTest where

import Control.Exception (catch)
import Grammar
import PrintStatement
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import GHC.IO.Exception (assertError)

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
    (parse printStatementParser "" "print \"Hello $\"")
    (Right (PrintStatement (Literal (StringLiteral "Hello $"))))

testCharPrintStatementParser :: Assertion
testCharPrintStatementParser =
  assertEqual
    "testCharPrintStatementParser"
    (parse printStatementParser "" "print 'a'")
    (Right (PrintStatement (Literal (CharacterLiteral 'a'))))

testNumberCharPrintStatementParser :: Assertion
testNumberCharPrintStatementParser =
  assertEqual
    "testNumberCharPrintStatementParser"
    (parse printStatementParser "" "print '1'")
    (Right (PrintStatement (Literal (CharacterLiteral '1'))))

printStatementTest :: TestTree
printStatementTest =
  testGroup
    "Builder Function Tests"
    [ testCase "testStringWithSpacePrintStatementParser" testStringWithSpacePrintStatementParser,
      testCase "testSingleStringPrintStatementParser" testSingleStringPrintStatementParser,
      testCase "testSpecialCharacterStringPrintStatementParser" testSpecialCharacterStringPrintStatementParser,
      testCase "testSpecialCharacterStringSpacePrintStatementParser" testSpecialCharacterStringSpacePrintStatementParser,
      testCase "testCharPrintStatementParser" testCharPrintStatementParser,
      testCase "testNumberCharPrintStatementParser" testNumberCharPrintStatementParser
    ]