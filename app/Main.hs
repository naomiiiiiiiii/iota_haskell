module Main (main) where

import qualified Interpreter.Interpreter as Interp
import qualified Interpreter.Environment as Env

import qualified Data.IntMap as M
import Control.Monad.State.Strict

-- run the interpreter, starting with an empty store and empty global and local environments
main :: IO ()
main = fst <$> runStateT Interp.interpret (M.empty, Env.emptyEnv)
