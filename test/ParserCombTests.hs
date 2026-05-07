
module ParserCombTests (tests) where

import qualified Parser.ParsingCombinators as PC
import Lexer.Lexer (Token(..))

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit(testCase, assertEqual, assertBool)
import Data.Either (isLeft)

-- Each of these tests is a small example focussed on the functionality of one parsing combinator
tests :: TestTree
tests = testGroup "Parsing Combinator tests" [ident, key, intP]

-- TODO should check that all these combinators fail when expected as well
ident :: TestTree
ident = testCase "@ident@ parsing combinator" $
  do
    assertEqual "Succeeds when expected" (Right ("one", [Id "two",Id "three",Id "four"])) (PC.ident [Id "one",Id "two",Id "three",Id "four"])
    assertBool "Fails when expected" (isLeft (PC.ident [Key ".", Id "one"]))

key :: TestTree
key = testCase "@key@ parsing combinator" $
  assertEqual "Succeeds when expected" (Right ("->", [Key "->", Key "-"])) (PC.key "->" [Key "->", Key "->",Key "-"])

intP :: TestTree
intP = testCase "@intP@ parsing combinator" $
  assertEqual "Succeeds when expected" (Right (-42, [Int 1])) (PC.intP [Int (-42), Int 1])
