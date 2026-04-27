--Some generic parsing combinators as described in *ML for the Working Programmer*/
-- If a parsing combinator @p@ is built to parse @Lex.Token@s into values of type @A@, @p:: [Lex.Token] -> Either SyntaxError (A, [Lex.Token])@. If the parsing combinator fails to parse an @a@, it will produce a syntax error. If it succeeds, it will produce a tuple. The first value in the tuple is the result of the parsing. The second value is the remaining, yet unparsed Lex.Tokens.

module Parser.ParsingCombinators (ident, key, intP, unitP, epsilon, (|:|), force, circ, keyCircR, keyCircL, (>>>), repeatP, parse)
  where

import qualified Lexer.Lexer as Lex

------------
--- Error handling
------------

-- @SyntaxError@ is a type that the parsing combinator can return if it wants to error out with a particular message.
-- An error can be caught and handled by the caller parser unless it is forced with "forceError"
-- If the error is not caught, it will get to the toplevel parsing function (@parse@) and the parser will error out.
data SyntaxError = SyntaxError String

-- throw an error, making it clear that it comes from the ParsingCombinators file
pcError :: String -> a
pcError msg = error ("ParsingCombinators." ++ msg)

------------
--- Parsing combinators
------------

-- @ident toks@ parses the first identifier in @toks@
-- Raises syntax error if @toks@ does not begin with an identifier
ident :: [Lex.Token] -> Either SyntaxError (String, [Lex.Token])
ident toks = case toks of
          (Lex.Id s):remToks -> Right (s, remToks)
          _ -> Left $ SyntaxError ("ident: expected identifier Lex.Token, got " ++ (show toks))

-- If @toks@ begins with @Key k@, @key toks@ parses the keyword @k@
-- Raises syntax error otherwise
key :: String -> [Lex.Token] -> Either SyntaxError (String, [Lex.Token])
key k toks = case toks of
               (Lex.Key k0):remToks | (k == k0) -> Right (k, remToks)
               _ -> Left $ SyntaxError ("key: expected keyword" ++ k ++ ", got " ++ (show toks))

-- If @toks@ begins with @Int i@, @intP toks@ parses the integer
-- Raises syntax error otherwise
intP :: [Lex.Token] -> Either SyntaxError (Int, [Lex.Token])
intP toks = case toks of
              (Lex.Int i):remToks -> Right (i, remToks)
              _  -> Left $ SyntaxError ("intP: expected int, got " ++ (show toks))

-- If @toks@ begins with unit, @unitP toks@ parses the unit
-- Raises syntax error otherwise
unitP :: [Lex.Token] -> Either SyntaxError ((), [Lex.Token])
unitP toks = case toks of
              (Lex.Key "("):(Lex.Key ")"):remToks -> Right ((), remToks)
              _  -> Left $ SyntaxError ("unitP: expected (), got " ++ (show toks))

-- @epsilon toks@ = @([], toks)@
-- @epsilon@ can be used as the base case in recursive parsing combinators that return lists (for example, @repeat@)
epsilon :: [Lex.Token] -> Either SyntaxError ([a], [Lex.Token])
epsilon = Right . (,) []

-- For two parsing combinators @p1@ and @p2@,
-- @(p1 |:| p2) toks@ tries @p1@ on @toks@. If @p1@ succeeds, it returns the output of @p1@ on @toks@. If @p1@ fails, @(p1 |:| p2)@ returns the output of @p2@ on @toks@.
(|:|) :: ([Lex.Token] -> Either SyntaxError pair) ->
         ([Lex.Token] -> Either SyntaxError pair) ->
         [Lex.Token] -> Either SyntaxError pair
(|:|) p1 p2 toks = case p1 toks of
                     Right out -> Right out
                     Left _err -> p2 toks

-- For a parsing combinator @p@, @force p toks@ runs @p@ on @toks@. If @p@ fails, the entire parser is guaranteed to fail.
force :: ([Lex.Token] -> Either SyntaxError pair ) ->
         [Lex.Token] -> pair
force p toks = case p toks of
                 Right out -> out
                 Left (SyntaxError errMsg) -> pcError ("forced " ++ errMsg)

-- @circ@ is the parsing-combinator version of function composition
-- For parsing combinator @p2@ that parses into type @b@ and @p1@ that parses into type @a@, @circ p2 p1@ will first run @p1@ to produce @(resA, midToks)@. @circ@ will then run @p2@ on @midToks@ to get @(resB, remToks@). Finally, @circ@ will return the composed result @((resA, resB), remToks)@
circ :: ([Lex.Token] -> Either SyntaxError (b, [Lex.Token])) ->
        ([Lex.Token] -> Either SyntaxError (a, [Lex.Token])) ->
        [Lex.Token] -> Either SyntaxError ((a, b), [Lex.Token])
circ p2 p1 toks = do
  (resA, midToks) <- p1 toks
  (resB, remToks) <- p2 midToks
  return ((resA, resB), remToks)

-- For parsing combinator @p@ and string @k@,
-- @keyCircR p k toks@ first parses the keyword k from @toks@. Then, it applies @p@ to the remaining tokens and returns the result.
-- @keyCircR p k@ is equivalent to @circ p (key k)@, but it discards the result of @(key k)@
-- It's called @keyCircR@ because @p@ is applied to the rightmost part of @toks, after the keyword is removed.
keyCircR :: ([Lex.Token] -> Either SyntaxError (a, [Lex.Token])) -> String ->
            [Lex.Token] -> Either SyntaxError (a, [Lex.Token])
keyCircR p k = (circ p (key k)) >>> snd

-- For parsing combinator @p@ and string @k@,
-- @keyCircL k p toks@ first applies @p@ to @toks@ with result @(res, midToks)@. It then parses the keyword k from @midToks@, to get @(_, remToks)@. Finally, it returns @(res, remToks)@
-- @keyCircL p k@ is equivalent to @circ (key k) p@, but it discards the result of @(key k)@
-- It's called @keyCircL@ because @p@ is applied to the leftmost part of @toks, before the keyword is removed.
keyCircL :: String -> ([Lex.Token] -> Either SyntaxError (a, [Lex.Token])) ->
            [Lex.Token] -> Either SyntaxError (a, [Lex.Token])
keyCircL k p = (circ (key k) p) >>> fst

-- For parsing combinator @p@ and function @f@, @p >>> f@ applies @f@ to the result of @p@.
(>>>) :: ([Lex.Token] -> Either SyntaxError (a, [Lex.Token])) -> (a -> b) ->
         [Lex.Token] -> Either SyntaxError (b, [Lex.Token])
(>>>) p f toks = do
  (pRes, remToks) <- p toks
  return (f pRes, remToks)

-- For a parsing combinator @p@ and token list @toks@,
-- @repeatP p toks@ applies @p@ to @toks@ until @p@ fails, collecting all the results in a list.
-- When @p@ fails, @repeatP@ does not fail as well. It simply stops and returns the collected results.
repeatP :: ([Lex.Token] -> Either SyntaxError (a, [Lex.Token])) -> [Lex.Token] -> Either SyntaxError ([a], [Lex.Token])
repeatP p = ((circ (repeatP p) p) >>> cons) |:| epsilon
  where
    cons = uncurry (:)

-- Top-level parsing function. Given a parsing combinator @p@ and a string to parse @s@, @parse@ will first lex @s@ (according to the provided @Lex.Keywords@), then apply @p@ to the lexed result.
parse :: Lex.Keywords -> ([Lex.Token] -> Either SyntaxError (a, [Lex.Token])) -> String -> a
parse keyWs p s = case p $ Lex.scan keyWs s of
                    Right (out, []) -> out -- Succesfully parsed all of @s@
                    Right(_, _r:_) -> pcError ("parse: Extra chars in phrase: " ++ s)
                    Left (SyntaxError msg) -> pcError msg -- SyntaxError was raised and not handled, so now the parser needs to fail.
