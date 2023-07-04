
import Test.Tasty
import CommentTest
import ErrorTest
import LiteralTest
import TypeTest
import ExpressionTest
import FunctionTest
import PrintStatementTest
import ProgramTest
import StatementTest
import ArrayTemplateTest
import CommentTemplateTest
import ExpressionTemplateTest
import FunctionTemplateTest
import PrintStatementTemplateTest
import LoopTemplateTest
import LiteralTemplateTest
import ProgramTemplateTest
import StatementTemplateTest
import VariableTemplateTest

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Builder Tests" [
    ArrayTemplateTest.arrayTemplateTest,
    PrintStatementTemplateTest.printStatementTemplateTest,
    CommentTemplateTest.commentTemplateTest,
    ExpressionTemplateTest.expressionTemplateTest,
    FunctionTemplateTest.functionTemplateTest,
    CommentTest.commentTests,
    ErrorTest.errorTests,
    LiteralTest.literalTests,
    TypeTest.typeTests,
    ExpressionTest.expressionTests,
    PrintStatementTest.printStatementTest,
    ProgramTest.programTest,
    StatementTest.statementTests,
    FunctionTest.functionTests,
    LoopTemplateTest.loopTemplateTests,
    LiteralTemplateTest.literalTemplateTests,
    ProgramTemplateTest.programTransformerTests,
    StatementTemplateTest.statementTransformerTests,
    VariableTemplateTest.variableDefinitionTemplateTests
    ]
