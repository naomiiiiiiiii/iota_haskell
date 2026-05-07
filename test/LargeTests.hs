{-# LANGUAGE ScopedTypeVariables#-}

module LargeTests (tests) where

import Parser.IotaParser
import qualified Interpreter.Interpreter as Interp
import AST as AST

import qualified Data.Map as M
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit(Assertion, testCase, assertEqual)

-- These tests run a relatively large example through the full pipeline (parsing, typechecking, reduction), and checks that each stage is correct

tests :: TestTree
tests = testGroup "Large, full-pipeline tests"
        (map (\exName -> testCase (exName ++ ": parse, typecheck, and reduce") (makeLargeTest exName)) ["Int reference"])

makeLargeTest :: String -> Assertion
makeLargeTest exName = do
  let exStrings = examples M.! exName
      exBindings :: [(String, AST.Exp)]= map parseIota exStrings
  assertEqual "Checking results of lexing and parsing" exBindings (parsingResults M.! exName)

-- @examples@ contains all the large examples we test on, organized by name
-- Each example is represented as a string list, one string for each let-binding
examples :: M.Map String [String]
examples = M.fromList [("Int reference", ["let test = bind(ret(5), \\n.bind(ref(0), \\r.bind(r:=n, \\u.bind(!r, \\m.ret m))))"])]

-- @parsingResults@ contains the expected results of parsing the large examples we test on, organized by name
-- The example consists of a list of let-bindings. Each let-binding in the example is parsed to get the LHS of the binding (a string name) and the RHS of the binding (an expression).
parsingResults :: M.Map String [(String, AST.Exp)]
parsingResults = M.fromList [("Int reference", [("test", Bind (Ret (Int 5),("n",Bind (Ref (Int 0),("r",Bind (Asgn (Bound 0,Bound 1),("u",Bind (Deref (Bound 1),("m",Ret (Bound 0))))))))))])]
