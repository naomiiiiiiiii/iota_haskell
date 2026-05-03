-- Toplevel interpretation function for iota
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Interpreter (interpret)
  where
import qualified Parser.IotaParser as P
import qualified Interpreter.Environment as Env
import qualified Interpreter.Typechecker as TC
import qualified Interpreter.Reducer as R
import qualified AST as AST

import System.IO (hFlush, stdout)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Prettyprinter
import qualified Data.IntMap as M

-- Run a REPL that interprets a sequence of Iota let-bindings (provided through @stdin@). After each let-binding, the REPL prints the resulting evaluated value and memory store.
-- The ambient state consists first of the global, mutable memory store (a map of locations to expressions) and second of the variable environment
interpret :: StateT (M.IntMap AST.Exp, Env.Env) IO ()
interpret = do
  lift $ putStr ">> "
  lift $ hFlush stdout
  inStr <- lift $ getLine
  let (name, expr) = P.parseIota inStr
  (initStore, initEnv) <- get
  let expType = runReader (TC.typeChecker expr) initEnv
  let (reducedExp, finalStore) = runReader
                                 (runStateT (R.reduce expr) initStore)
                                 initEnv
  lift $ putStrLn $ show $ prettyStep name expType reducedExp finalStore
  -- update the state to the new store
  modify (\(_, env) -> (finalStore, env))
 -- update the state to the new environment
  modify (\(store, oldEnv) -> (store, Env.addGlobalEnv name (expType, reducedExp) oldEnv))
  interpret
  where
    -- print the evaluation of a let-binding
    prettyStep :: String -> AST.Typ -> AST.Exp -> M.Map Int AST.Exp -> Doc ann
    prettyStep name tau v finalStore = pretty v
    --   (pretty name) <> ":" <> (pretty tau) <+> "=>*" <+> (pretty v) <\>
    --   "New store is:" <+> (pretty finalStore)
    -- (<\>) s1 s2 = s1 <> "\n" <> s2



