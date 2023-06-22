
import Test.Tasty
import CommentTest
import ErrorTest
import LiteralTest
import TypeTest
import ExpressionTest
import FunctionTest
import IfTest
import PrintStatementTest
import ProgramTest
import StatementTest

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Builder Tests" [
    CommentTest.commentTests,
    ErrorTest.errorTests,
    LiteralTest.literalTests,
    TypeTest.typeTests,
    ExpressionTest.expressionTests,
    IfTest.ifTests,
    PrintStatementTest.printStatementTest,
    ProgramTest.programTest,
    StatementTest.statementTests,
    FunctionTest.functionTests]
