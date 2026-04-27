module Main (main) where

import System.IO (hFlush, stdout)
import qualified Lexer.Lexer as Lex

main :: IO ()
main = do
  putStr ">> "
  hFlush stdout
  inStr <- getLine
  let tokList = Lex.scan Lex.iotaKeywords inStr
  putStrLn (show tokList)
  main
