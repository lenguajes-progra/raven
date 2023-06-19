
import Test.Tasty
import CommentTest
import ErrorTest
import LiteralTest
import TypeTest
import ExpressionTest

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Builder Tests" [
    CommentTest.commentTests,
    ErrorTest.errorTests,
    LiteralTest.literalTests,
    TypeTest.typeTests,
    ExpressionTest.expressionTests]