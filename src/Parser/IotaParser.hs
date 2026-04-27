module Parser.IotaParser (parseIota)
  where

import Parser.ParsingCombinators
import qualified AST as AST
import qualified Lexer.Lexer as Lex

------------
--- Parser combinators
------------
-- The first combinators are the smallest and parse the simplest parts of the AST. These simple combinators are combined later in the file to parse larger parts of the AST. Eventually, we can define @letBinding@, which parses a toplevel let-binding of the form @let [str] = [AST.Exp]@

-- parse constant value
constant :: [Lex.Token] -> Either SyntaxError (AST.Exp, [Lex.Token])
constant = (intP >>> AST.Int) |:| (unitP >>> (\_ -> AST.Star))

-- parse base types
baseTyp :: [Lex.Token] -> Either SyntaxError (AST.Typ, [Lex.Token])
baseTyp = ((key "Int") >>> (\_ -> AST.IntTyp)) |:| ((key "Unit") >>> (\_ -> AST.UnitTyp))

iotaParser :: ([Lex.Token] -> Either SyntaxError (a, [Lex.Token]))
iotaParser = error "TODO"

parseIota :: String -> AST.Exp
parseIota = parse Lex.iotaKeywords iotaParser
