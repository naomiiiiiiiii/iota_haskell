{-# LANGUAGE NamedFieldPuns#-}

-- A generic lexer implemented as described in *ML for the Working Programmer*/
module Lexer.Lexer (iotaScan)
where

import Data.Char (isAlpha, isAlphaNum, isPunctuation, isSymbol)
import Text.Read (readMaybe)

------------
--- Keywords
------------
data Keywords = Keywords {alphaNumeric :: [String] -- alpha-numeric keywords
                        , symbols :: [String] -- keywords with symbols (non-alpha-numeric) characters
                        , commentL :: Char
                        , commentR :: Char}

iotaKeywords :: Keywords
iotaKeywords = Keywords {alphaNumeric, symbols, commentL, commentR}
  where alphaNumeric = ["ret", "bind", "let", "ref", "in", "Nat", "Unit", "Ref", "Comp", "let"]
        symbols = ["(", ")", "\\", ".", "=", ":=", "!", "->", "+", "*", "-", "/"]
        commentL = '{' -- TODO, would be nice to have a multi-character comment intro like /* comment */
        commentR = '}'

------------
--- Lexer
------------

data Token = Id String -- Identifiers
           | Key String -- Keywords
           | Int Int -- Numbers
  deriving(Show, Eq)

-- @alphaTok@ takes a @Keywords@ value describing the keywords of the lexed language
-- and a @String@ of alphanumeric characters
-- @alphaTok@ scans the string into a keyword or an identity, depending on the @Keywords@
alphaTok :: Keywords -> String -> Token
alphaTok keywords str = if (all isAlphaNum str)
                        then if (elem str $ alphaNumeric keywords) then Key str else Id str
                        else error ("Lexer.alphaTok: Expected alphanumeric string, got " ++ str)

-- Lex an integer
intTok :: String -> Token
intTok str = (maybe
              (error $ "Lexer.intTok: Expected numeric string, got " ++ str)
              Int) . readMaybe $ str

-- @scanSymbol keywords front rem@ returns @(tok, rem') where @tok@ contains the shortest prefix of @front ++ rem@ that is also a (non-empty) symbolic token. @rem'@ is the remainder of @rem@ left unscanned.
-- In other words, @scanSymbol@ scans over @rem@ from left to right. If @front@ becomes a symbolic keyword OR it sees a non-symbolic character in @rem@, it stops scanning and returns @front@ as a symbolic token. Otherwise, it appends the new symbolic character to @front@ and recurses on the remainder of @rem@.
-- @scanSymbol@ assumes that @front@ is nonempty and comprised of symbols
scanSymbol :: Keywords -> String -> String -> (Token, String)
scanSymbol keyWs front back = do
  case back of
    [] -> (Key(front), back) -- @rem@ is empty, return all the symbols in @front@
    hd:remBack -> if ((elem front (symbols keyWs)) || not (isSymbolic hd))
                 then (Key(front), back)
                 else scanSymbol keyWs (front ++ [hd]) remBack
-- Line 47 is slow because of the singleton append. IDEA: build up front backwards, then reverse once at the end? Then I would have to reverse all members of @Keywords.symbols@ too for the comparison, so is it really faster?

------------
--- Lexer helpers
------------
isSymbolic :: Char -> Bool
isSymbolic s = isSymbol s || isPunctuation s
