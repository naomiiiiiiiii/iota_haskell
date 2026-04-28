module Parser.IotaParser (parseIota)
  where

import Parser.ParsingCombinators
import qualified AST as AST
import qualified Lexer.Lexer as Lex

------------
--- Parser combinators
------------
-- The first combinators are the smallest and parse the simplest parts of the AST. These simple combinators are combined later in the file to parse larger parts of the AST. Eventually, we can define @letBinding@, which parses a toplevel let-binding of the form @let [str] = [AST.Exp]@

-- parse constant values (Integers and Units)
constant :: [Lex.Token] -> Either SyntaxError (AST.Exp, [Lex.Token])
constant = (intP >>> AST.Int) |:| (unitP >>> (\_ -> AST.Star))

-- parse base types (Integer and Unit)
baseTyp :: [Lex.Token] -> Either SyntaxError (AST.Typ, [Lex.Token])
baseTyp = ((key "Int") >>> (\_ -> AST.IntTyp)) |:| ((key "Unit") >>> (\_ -> AST.UnitTyp))

-- parse any AST type
typ :: [Lex.Token] -> Either SyntaxError (AST.Typ, [Lex.Token])
typ = ((circ (keyCircR atomTyp "->") atomTyp) >>> AST.ArrowTyp) -- case for function types
      |:| atomTyp -- case for base types or types wrapped in parentheses
  where atomTyp = baseTyp
                   |:| (keyCircL ")" (keyCircR typ "("))
                   |:| ((keyCircR atomTyp "Ref") >>> AST.RefTyp) -- case for reference types
                   |:| ((keyCircR atomTyp "Comp") >>> AST.CompTyp) -- case for computations

-- parse a type-annotated variable
typedId :: [Lex.Token] -> Either SyntaxError ((String, AST.Typ), [Lex.Token])
typedId = keyCircL ")" (circ typ -- parse the type
                         (keyCircL ":" (keyCircR ident "("))) -- parse the identifier

-- parse an expression
-- expr proceeds by casing on the input list @toks@
-- The cases are separated by the @|:|@ operator -- when a case fails, @expr@ drops into the next case via @|:|@
expr :: [Lex.Token] -> Either SyntaxError (AST.Exp, [Lex.Token])
expr =
 -- First case : expr is a lambda
  (circ expr -- Looking for the body of the function
    (keyCircL "." -- Looking for the period that indicates no more arguments
      (keyCircR
        (repeatP typedId) -- Looking for a list of arguments like "(x : Int)(y: Int -> Int)"
        "\\")) -- Looking for a lambda "\"
    >>> AST.absList) -- Combine the argument list and the body into an @AST.Exp@
  |:| ((keyCircR atom "!") >>> AST.Deref)
  |:| ((keyCircR atom "ret") >>> AST.Ret)
  |:| ((circ (repeatP atom) atom) >>> AST.applyList) -- a single atomic expression or an application of one atom to other atom(s). At least one expression must be present here, the empty string should not parse as an expression
  -- Atomic expressions
  where atom = (ident >>> AST.Free)
               |:| constant
               |:| (keyCircL ")" (keyCircR expr "("))


iotaParser :: ([Lex.Token] -> Either SyntaxError (a, [Lex.Token]))
iotaParser = error "TODO"

parseIota :: String -> AST.Exp
parseIota = parse Lex.iotaKeywords iotaParser
