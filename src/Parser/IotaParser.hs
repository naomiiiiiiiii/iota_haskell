module Parser.IotaParser (parseIota)
  where

import Parser.ParsingCombinators
import qualified AST as AST
import qualified Lexer.Lexer as Lex

iotaParser :: ([Lex.Token] -> Either SyntaxError (a, [Lex.Token]))
iotaParser = error "TODO"

parseIota :: String -> AST.Exp
parseIota = parse Lex.iotaKeywords iotaParser
