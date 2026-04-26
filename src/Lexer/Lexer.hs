{-# LANGUAGE NamedFieldPuns#-}

-- A generic lexer implemented as described in *ML for the Working Programmer*/
module Lexer.Lexer (iotaScan)
where

import Data.Char (isAlpha, isAlphaNum)
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
