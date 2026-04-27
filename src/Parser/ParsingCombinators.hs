--Some generic parsing combinators as described in *ML for the Working Programmer*/
-- If a parsing combinator @p@ is built to parse @Lex.Token@s into values of type @A@, @p:: [Lex.Token] -> Either SyntaxError (A, [Lex.Token])@. If the parsing combinator fails to parse an @a@, it will produce a syntax error. If it succeeds, it will produce a tuple. The first value in the tuple is the result of the parsing. The second value is the remaining, yet unparsed Lex.Tokens.

module Parser.ParsingCombinators (parse)
  where

import qualified Lexer.Lexer as Lex

------------
--- Error handling
------------

-- @SyntaxError@ is a type that the parsing combinator can return if it wants to error out with a particular message.
-- An error can be caught and handled by the caller parser unless it is forced with "forceError"
-- If the error is not caught, it will get to the toplevel parsing function (@parse@) and the parser will error out.
data SyntaxError = SyntaxError String

-- force the provided @SyntaxError@ to run, causing the parser to fail immediately
forceError :: SyntaxError -> a
forceError (SyntaxError msg) = error msg

------------
--- Parsing combinators
------------

-- Top-level parsing function. Given a parsing combinator @p@ and a string to parse @s@, @parse@ will first lex @s@ (according to the provided @Lex.Keywords@), then apply @p@ to the lexed result.
parse :: Lex.Keywords -> ([Lex.Token] -> Either SyntaxError (a, [Lex.Token])) -> String -> a
parse keyWs p s = case p $ Lex.scan keyWs s of
                    Right (e, []) -> e -- Succesfully parsed all of @s@
                    Right(e, _r:_) -> error ("Extra chars in phrase:: " ++ s ++ "\n")
                    Left (SyntaxError msg) -> error msg -- SyntaxError was raised and not handled, so now the parser needs to fail.
