import Test.Tasty (defaultMain, testGroup, TestTree)
import qualified LargeTests as LargeTests
import qualified ParserCombTests as PCTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [LargeTests.tests, unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [PCTests.tests] -- TODO add unit tests for iota's parser, lexer, typechecking, reduction
