{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ParserCombTests (tests) where

import qualified Parser.ParsingCombinators as PC
import Lexer.Lexer as Lex (Token(..))

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit(testCase, assertEqual, assertBool)
import Data.Either (isLeft)
import Data.Char (toUpper)
import Data.List (singleton)
import Control.Exception (try, evaluate, ErrorCall)
import qualified Control.DeepSeq as DeepSeq (force)

-- Each of these tests is a small example focussed on the functionality of one parsing combinator
tests :: TestTree
tests = testGroup "Parsing Combinator tests" [ident, key, intP, unitP, orP, force, circ, keyCircR, keyCircL, pipe, repeatP]

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

unitP :: TestTree
unitP = testCase "@unitP@ parsing combinator" $
  assertEqual "Succeeds when expected" (Right ((), [Key "(", Key ")"])) (PC.unitP [Key "(", Key ")", Key "(", Key ")"])

orP :: TestTree
orP = testCase "@|:|@ parsing combinator" $ do
  (assertEqual
    "Chooses LHS when possible"
    (Right ("lhs", [Id "end"]))
    (parseIntoLhs PC.|:| parseIntoRhs $ [Id "start", Id "end"]))
  (assertEqual
    "Chooses RHS otherwise"
    (Right ("rhs", [Id "end"]))

    (failParse PC.|:| parseIntoRhs $ [Id "start", Id "end"]))
  where
    parseIntoLhs = \case
      _:xs -> Right ("lhs", xs)
      [] -> Left $ PC.SyntaxError "parseIntoLhs"
    parseIntoRhs = \case
      _:xs -> Right ("rhs", xs)
      [] -> Left $ PC.SyntaxError "parseIntoRhs"

force :: TestTree
force = testCase "@force@ parsing combinator" $ do
  out :: Either ErrorCall (Either PC.SyntaxError (String, [Token])) <-
    try $ evaluate $ DeepSeq.force $
    ((Right . (PC.force failParse)) PC.|:| PC.ident) [Id "start", Id "end"]
  assertBool "Failure propagates through" (isLeft out)

circ :: TestTree
circ = testCase "@circ@ parsing combinator" $
  assertEqual "Succeeds when expected"
  (Right (("start", "\\"), [Key "\\"]))
  (PC.circ (PC.key "\\") PC.ident [Id "start", Key "\\", Key "\\"])

keyCircR :: TestTree
keyCircR = testCase "@keyCircR@ parsing combinator" $
  assertEqual "Succeeds when expected"
  (Right ((), [Key ")"]))
  (PC.keyCircR PC.unitP "(" [Key "(", Key "(", Key ")", Key ")"])

keyCircL :: TestTree
keyCircL = testCase "@keyCircL@ parsing combinator" $
  assertEqual "Succeeds when expected"
  (Right ((), [Key ")"]))
  (PC.keyCircL ")" PC.unitP [Key "(", Key ")", Key ")", Key ")"])

pipe :: TestTree
pipe = testCase "@(>>>)@ parsing combinator" $
  assertEqual "Succeeds when expected"
  (Right ("HELLO", [Id "world"]))
  (PC.ident PC.>>> (map toUpper) $ [Id "hello", Id "world"])

repeatP :: TestTree
repeatP = testCase "@repeatP@ parsing combinator" $ do
  assertEqual "Repeats many times"
    (Right (map singleton "hello world", [Key "*"]))
    (PC.repeatP PC.ident $ (map (Id . singleton) "hello world") ++ [Key "*"]) -- the second argument to @repeatP@ is a list of identifiers (where each identifier is a single-character string) followed by @Key "*"@. Together, all the identifiers spell "hello world"
  assertEqual "Succeeds when argument parser cannot match"
    (Right ([], [Key "*"]))
    (PC.repeatP PC.ident [Key "*"])

-- Parser that always fails
failParse :: [Lex.Token] -> Either PC.SyntaxError (String, [Lex.Token])
failParse = \_ -> Left $ PC.SyntaxError "failParse"
