module Main (main) where

import System.IO (hFlush, stdout)
import qualified Lexer.Lexer as Lexer

main :: IO ()
main = do
  putStr ">> "
  hFlush stdout
  inStr <- getLine
  let tokList = Lexer.iotaScan inStr
  putStrLn (show tokList)
  main
