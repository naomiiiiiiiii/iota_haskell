-- Toplevel interpretation function for iota
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Interpreter (interpret, interpretBindings)
  where
import qualified Parser.IotaParser as P
import qualified Interpreter.Environment as Env
import qualified Interpreter.Typechecker as TC
import qualified Interpreter.Reducer as R
import qualified AST as AST

import System.IO (hFlush, stdout)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Functor.Identity (runIdentity)
import Prettyprinter

-- Interpret a let-binding (@let [NAME] = [EXP]@) and update the state and global environment
-- Returns the value and type of the bound variable
interpretBinding :: (String, AST.Exp) -> State (Env.Store, Env.Env) (AST.Exp, AST.Typ)
interpretBinding (name, expr) = do
  (initStore, initEnv) <- get
  let expType = runReader (TC.typeChecker expr) initEnv
  let (reducedExp, finalStore) = runReader
                                 (runStateT (R.reduce expr) initStore)
                                 initEnv
  -- update the state to the new store
  modify (\(_, env) -> (finalStore, env))
  -- update the state to the new environment
  modify (\(store, oldEnv) -> (store, Env.addGlobalEnv name (expType, reducedExp) oldEnv))
  return (reducedExp, expType)

-- Run a REPL that interprets a sequence of Iota let-bindings (provided through @stdin@). After each let-binding, the REPL prints the resulting evaluated value and memory store.
-- The ambient state consists first of the global, mutable memory store (a map of locations to expressions) and second of the variable environment
interpret :: StateT (Env.Store, Env.Env) IO ()
interpret = do
  lift $ putStr ">> "
  lift $ hFlush stdout
  inStr <- lift $ getLine
  let (name, expr) = P.parseIota inStr
  -- Interpret the binding (let @name@ = @exp@) and update the state
  (reducedExp, expType) <- mapStateT (return . runIdentity) (interpretBinding (name, expr))
  finalStore <- fst <$> get
  lift $ putStrLn $ show $ prettyStep name expType reducedExp finalStore
  interpret
  where
    -- print the evaluation of a let-binding
    prettyStep :: String -> AST.Typ -> AST.Exp -> Env.Store -> Doc ann
    prettyStep name tau v finalStore =
      -- TODO: print tau as an atom
      (pretty name) <> ":" <> (pretty tau) <+> "=>*" <+> (pretty v) <\>
      "New store is:" <+> (pretty finalStore)
    (<\>) s1 s2 = s1 <> "\n" <> s2


-- Interpret a list of let-bindings in order
-- Returns the value and type of each bound variable, in order
interpretBindings :: [(String, AST.Exp)] -> [(AST.Exp, AST.Typ)]
interpretBindings bindings = fst $ runState (mapM interpretBinding bindings) (Env.emptyStore, Env.emptyEnv)

