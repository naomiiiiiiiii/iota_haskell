module Main (main) where

import qualified Interpreter.Interpreter as Interp
import qualified Interpreter.Environment as Env

import qualified Data.Map as M
import Control.Monad.State

-- run the interpreter, starting with an empty store and empty global and local environments
main :: IO ()
main = fst <$> runStateT Interp.interpret (M.empty, Env.emptyEnv)
