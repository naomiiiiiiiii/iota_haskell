
module ParserCombTests (tests) where

import Parser.ParsingCombinators

import qualified Data.Map as M
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit(Assertion, testCase, assertEqual)

-- Each of these tests is a small example focussed on the functionality of one parsing combinator
tests :: TestTree
tests = testGroup "Parsing Combinator tests" []
