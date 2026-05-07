{-# LANGUAGE ScopedTypeVariables#-}

module LargeTests (tests) where

import Parser.IotaParser
import qualified Interpreter.Interpreter as Interp
import AST as AST

import qualified Data.Map as M
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit(Assertion, testCase, assertEqual)

-- Each of these tests run a relatively large example through the full pipeline (parsing, typechecking, reduction), and checks that each stage is correct

tests :: TestTree
tests = testGroup "Large, full-pipeline tests"
        (map (\exName -> testCase (exName ++ ": parse, typecheck, and reduce") (makeLargeTest exName)) (M.keys examples))

makeLargeTest :: String -> Assertion
makeLargeTest exName = do
  let exStrings = examples M.! exName
      exBindings :: [(String, AST.Exp)]= map parseIota exStrings
  assertEqual "Checking results of lexing and parsing" exBindings (parsingResults M.! exName)
  let (exVals, exTypes) = unzip $ Interp.interpretBindings exBindings
  assertEqual "Checking results of typechecking" exTypes (typingResults M.! exName)
  assertEqual "Checking results of reduction" exVals (reduceResults M.! exName)

-- @examples@ contains all the large examples we test on, organized by name
-- Each example is represented as a string list, one string for each let-binding
examples :: M.Map String [String]
examples = M.fromList
  [("Int reference (value 5)", [ "let test = bind(ret(5), \\n.bind(ref(0), \\r.bind(r:=n, \\u.bind(!r, \\m.ret m))))"])
 , ("Int reference (value -42)", ["let rneg42 = bind(ref(-1), \\r.bind(r := -42, \\u.bind(!r, \\m.ret m)))"])
 , ("Lambdas", ["let second = \\(x : Int).\\(y : Int).\\(z : Int).y + 100"
              , "let out = second 1 2 3"])]

-- @parsingResults@ contains the expected results of parsing the large examples we test on, organized by name
-- The example consists of a list of let-bindings. Each let-binding in the example is parsed to get the LHS of the binding (a string name) and the RHS of the binding (an expression).
parsingResults :: M.Map String [(String, AST.Exp)]
parsingResults = M.fromList [
  ("Int reference (value 5)", [("test", Bind (Ret (Int 5),("n",Bind (Ref (Int 0),("r",Bind (Asgn (Bound 0,Bound 1),("u",Bind (Deref (Bound 1),("m",Ret (Bound 0))))))))))])
 ,("Int reference (value -42)", [("rneg42",Bind (Ref (Int (-1)),("r",Bind (Asgn (Bound 0,Int (-42)),("u",Bind (Deref (Bound 1),("m",Ret (Bound 0))))))))])
 ,("Lambdas", [("second",Lam (("x",IntTyp),Lam (("y",IntTyp),Lam (("z",IntTyp),Binop (Plus,Bound 1,Int 100)))))
              ,("out",Ap (Ap (Ap (Free "second",Int 1),Int 2),Int 3))])
  ]

-- @typingResults@ contains the expected results of typing the large examples we test on, organized by name. There is one type for each let-binding in the large example.
typingResults :: M.Map String [AST.Typ]
typingResults = M.fromList [
  ("Int reference (value 5)", [CompTyp IntTyp])
 ,("Int reference (value -42)", [CompTyp IntTyp])
 ,("Lambdas", [ArrowTyp (IntTyp,ArrowTyp (IntTyp,ArrowTyp (IntTyp,IntTyp)))
              ,IntTyp])]

-- @reduceResults@ contains the expected results of reducing the large examples we test on, organized by name. There is one value for each let-binding in the large example.
reduceResults :: M.Map String [AST.Exp]
reduceResults = M.fromList [
  ("Int reference (value 5)", [Ret (Int 5)])
 ,("Int reference (value -42)", [Ret (Int $ -42)])
 ,("Lambdas", [Lam (("x",IntTyp),Lam (("y",IntTyp),Lam (("z",IntTyp),Binop (Plus,Bound 1,Int 100))))
              ,Int 102])]
