import Test.Tasty (defaultMain, testGroup, TestTree)
import qualified LargeTests as LargeTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [LargeTests.tests, unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [] -- TODO
