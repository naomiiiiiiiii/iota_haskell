{-# LANGUAGE NamedFieldPuns#-}

-- A generic lexer implemented as described in *ML for the Working Programmer*/
module Lexer.Lexer (iotaScan)
where


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

