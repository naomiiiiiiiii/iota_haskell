--Some generic parsing combinators as described in *ML for the Working Programmer*/
-- If a parsing combinator @p@ is built to parse @Lex.Token@s into values of type @A@, @p:: [Lex.Token] -> Either SyntaxError (A, [Lex.Token])@. If the parsing combinator fails to parse an @a@, it will produce a syntax error. If it succeeds, it will produce a tuple. The first value in the tuple is the result of the parsing. The second value is the remaining, yet unparsed Lex.Tokens.

module Parser.ParsingCombinators (ident, key, intP, unitP, epsilon, (|:|), force, circ, keycircr, keycircl, (>>>), repeat, parse)
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

-- force the provided @SyntaxError@ to run, causing the caller to fail immediately
forceError :: SyntaxError -> a
forceError (SyntaxError msg) = pcError msg

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

epsilon :: [Lex.Token] -> Either SyntaxError ([a], [Lex.Token])
epsilon toks = error "TODO"

(|:|) :: ([Lex.Token] -> Either SyntaxError pair) -> ([Lex.Token] -> Either SyntaxError pair) -> [Lex.Token] -> Either SyntaxError pair
(|:|) = error "TODO"

force :: ([Lex.Token] -> Either SyntaxError (a, [Lex.Token]) ) ->
         [Lex.Token] -> (a, [Lex.Token])
force = error "TODO"

circ :: ([Lex.Token] -> Either SyntaxError (b, [Lex.Token])) ->
        ([Lex.Token] -> Either SyntaxError (a, [Lex.Token])) ->
        [Lex.Token] -> Either SyntaxError ((a, b), [Lex.Token])
circ = error "TODO"

keycircr :: ([Lex.Token] -> Either SyntaxError (a, [Lex.Token])) -> String ->
            [Lex.Token] -> Either SyntaxError (a, [Lex.Token])
keycircr = error "TODO"

keycircl :: String -> ([Lex.Token] -> Either SyntaxError (a, [Lex.Token])) ->
            [Lex.Token] -> Either SyntaxError (a, [Lex.Token])
keycircl = error "TODO"

(>>>) :: ([Lex.Token] -> Either SyntaxError (a, [Lex.Token])) -> (a -> b) ->
         [Lex.Token] -> Either SyntaxError (b, [Lex.Token])
(>>>) = error "TODO"

-- Top-level parsing function. Given a parsing combinator @p@ and a string to parse @s@, @parse@ will first lex @s@ (according to the provided @Lex.Keywords@), then apply @p@ to the lexed result.
parse :: Lex.Keywords -> ([Lex.Token] -> Either SyntaxError (a, [Lex.Token])) -> String -> a
parse keyWs p s = case p $ Lex.scan keyWs s of
                    Right (out, []) -> out -- Succesfully parsed all of @s@
                    Right(_, _r:_) -> pcError ("parse: Extra chars in phrase: " ++ s)
                    Left (SyntaxError msg) -> pcError msg -- SyntaxError was raised and not handled, so now the parser needs to fail.
