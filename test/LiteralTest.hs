module LiteralTest where

import Literal
import Grammar
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import Control.Exception (catch)

testLiteralParserBooleanTrue :: Assertion
testLiteralParserBooleanTrue = assertEqual "parseLiteralBooleanTrue" (parse literalParser "" "true;") (Right (BooleanLiteral True))

testLiteralParserBooleanFalse :: Assertion
testLiteralParserBooleanFalse = assertEqual "parseLiteralBooleanFalse" (parse literalParser "" "false, true") (Right (BooleanLiteral False))

testLiteralParserIntPositive :: Assertion
testLiteralParserIntPositive = assertEqual "parseLiteralIntPositive" (parse literalParser "" "457") (Right (IntegerLiteral 457))

testLiteralParserIntNegative :: Assertion
testLiteralParserIntNegative = assertEqual "parseLiteralIntNegative" (parse literalParser "" "-457") (Right (IntegerLiteral (-457)))

testLiteralParserCharacter :: Assertion
testLiteralParserCharacter = assertEqual "parseLiteralCharacter" (parse literalParser "" "\'a\'") (Right (CharacterLiteral 'a'))

testLiteralParserCharNumber :: Assertion
testLiteralParserCharNumber = assertEqual "parseLiteralCharNumber" (parse literalParser "" "\'1\'") (Right (CharacterLiteral '1'))

testLiteralParserString :: Assertion
testLiteralParserString = assertEqual "parseLiteralString" (parse literalParser "" "\"This is a String\"") (Right (StringLiteral "This is a String"))

literalTests :: TestTree
literalTests = testGroup "Builder Literal Tests" [
    testCase "parseLiteralBooleanTrue" testLiteralParserBooleanTrue,
    testCase "parseLiteralBooleanFalse" testLiteralParserBooleanFalse,
    testCase "parseLiteralIntPositive" testLiteralParserIntPositive,
    testCase "parseLiteralIntNegative" testLiteralParserIntNegative,
    testCase "parseLiteralCharacter" testLiteralParserCharacter,
    testCase "parseLiteralCharNumber" testLiteralParserCharNumber,
    testCase "parseLiteralString" testLiteralParserString]