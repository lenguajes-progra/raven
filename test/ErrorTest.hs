module ErrorTest where

import Error
import Grammar
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec
import Control.Exception (catch)


testErrorParserSyntax :: Assertion
testErrorParserSyntax = assertEqual "syntaxError" (parse errorParser "" "Syntax error ") (Right (ErrorType Syntax))

testErrorParserSyntaxSemicolon :: Assertion
testErrorParserSyntaxSemicolon = assertEqual "syntaxErrorSemicolon" (parse errorParser "" "Syntax error;") (Right (ErrorType Syntax))

testErrorParserType :: Assertion
testErrorParserType = assertEqual "typeError" (parse errorParser "" "Type error ") (Right (ErrorType Type))

testErrorParserTypeSemicolon :: Assertion
testErrorParserTypeSemicolon = assertEqual "typeErrorSemicolon" (parse errorParser "" "Type error;") (Right (ErrorType Type))

testErrorParserTypeFunction :: Assertion
testErrorParserTypeFunction = assertEqual "typeFunctionError" (parse errorParser "" "Function Type error ") (Right (ErrorType TypeFunction))

testErrorParserTypeFunctionSemicolon :: Assertion
testErrorParserTypeFunctionSemicolon = assertEqual "typeFunctionErrorSemicolon" (parse errorParser "" "Function Type error;") (Right (ErrorType TypeFunction))

testErrorParserAssignType :: Assertion
testErrorParserAssignType = assertEqual "assignTypeError" (parse errorParser "" "Assign Type error ") (Right (ErrorType AssignType))

testErrorParserAssignTypeSemicolon :: Assertion
testErrorParserAssignTypeSemicolon = assertEqual "assignTypeErrorSemicolon" (parse errorParser "" "Assign Type error;") (Right (ErrorType AssignType))

errorTests :: TestTree
errorTests = testGroup "Builder Error Tests" [
    testCase "syntaxError" testErrorParserSyntax,
    testCase "syntaxErrorSemicolon" testErrorParserSyntaxSemicolon,
    testCase "typeError" testErrorParserType,
    testCase "typeErrorSemicolon" testErrorParserTypeSemicolon,
    testCase "typeFunctionError" testErrorParserTypeFunction,
    testCase "typeFunctionErrorSemicolon" testErrorParserTypeFunctionSemicolon,
    testCase "assignTypeError" testErrorParserAssignType,
    testCase "assignTypeErrorSemicolon" testErrorParserAssignTypeSemicolon]