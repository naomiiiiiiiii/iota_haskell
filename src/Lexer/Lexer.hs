{-# LANGUAGE NamedFieldPuns#-}

-- A generic lexer implemented as described in *ML for the Working Programmer*/ (@scan@)
-- Also an instantiation of the generic lexer to iota (@iotaScan@)
module Lexer.Lexer (Keywords, Token(..), scan, iotaScan)
where

import Data.Char (isAlpha, isAlphaNum, isDigit, isPunctuation, isSymbol, isPrint, isSpace)
import Data.List (uncons)
import Text.Read (readMaybe)

------------
--- Keywords
------------
data Keywords = Keywords {alphaNumeric :: [String] -- alpha-numeric keywords
                        , symbols :: [String] -- keywords with symbols (non-alpha-numeric) characters
                        , commentL :: Char
                        , commentR :: Char}
data KeywordType = CommentL
                 | Alpha
                 | Number
                 | Symbol
                 | NonGraphical

-- @whichKeyword keywords char@ returns what type of keyword can begin with @char@
whichKeyword :: Keywords -> Char -> KeywordType
whichKeyword keywords char =
  if (char == commentL keywords) then CommentL
  else if (isAlpha char) then Alpha
  else if (isDigit char || char == '-') then Number -- @char@ indicates the beginning of a number
  else if (isSymbolic char) then Symbol
  else if (not $ isGraphical char) then NonGraphical -- @char@ is whitespace or other irrelevant chararacter
  else error ("Lexer.whichKeyword : Could not lex character " ++ [char])

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

-- @scan keywords str@ will lex @str@ into a list of @Tokens@, using the keywords described in @keywords@. It does this by casing on whether @str@ is a comment, an alphanumeric character, a number, or a symbol.
-- In ML, @val scan: string -> token list@ because @Keywords@ is quantified at the
-- module level. All lexer functions should depend on the SAME @Keywords@
scan :: Keywords -> String -> [Token]
scan keywords = scanHelp []
  where
    scanHelp :: [Token] -> String -> [Token]
    scanHelp toks s = case (uncons s) of
      Nothing -> reverse toks -- We have successfully lexed @s@
      Just (sHead, sTail) -> -- Start by examining the first character of @s@
        let (newToks, newS) =
              -- Case on what type of keyword begins with @sHead@
              case (whichKeyword keywords sHead) of
                CommentL ->
                  let commentRemoved = dropWhile (/= (commentR keywords)) sTail in
                  -- Line above: drop the comment from the string we are lexing
                    (toks, dropWhile (== (commentR keywords)) commentRemoved)
                -- Line above: drop the comment terminator from the string we are lexing
                -- Case below: we are lexing an alpha-numeric identifier or alphabetical keyword
                Alpha -> let (alphas, remS) = span isAlphaNum s in
                           ((alphaTok keywords alphas):toks, remS)
                Number -> let (num, remSTail) = span isDigit sTail in -- gather all digits from the front of @s@, excluding @sHead@, which could be a minus-sign
                            (intTok(sHead:num):toks, remSTail) -- add @sHead@ back, pass to @intTok@
                Symbol -> let (tok, remS) = scanSymbol keywords [sHead] sTail in
                            (tok:toks, remS)
                -- @sHead@ is whitespace or other irrelevant char
                NonGraphical -> (toks, dropWhile (not . isGraphical) s) -- remove all irrelevant chars
        in
          scanHelp newToks newS

iotaScan :: String -> [Token]
iotaScan = scan iotaKeywords

------------
--- Lexer helpers
------------
isSymbolic :: Char -> Bool
isSymbolic s = isSymbol s || isPunctuation s

isGraphical :: Char -> Bool
isGraphical s = isPrint s && (not $ isSpace s)
