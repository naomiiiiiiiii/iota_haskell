{-# LANGUAGE NamedFieldPuns#-}

-- An environment of global and local Iota expression bindings, useful in
-- typechecking
module Interpreter.Environment (Env(..), newEnv, addGlobalEnv, emptyEnv)
  where

import qualified AST as AST
import qualified Data.Map as M


-- When interpretting an expression, we have recourse to two environments
-- The first is all previous global expression declarations, represented as a map from strings (variable names) to typed expressions.
-- The second is all local bindings (introduced by lambdas), represented as a simple list of types. Lists work nicely with local bindings' De Bruijn indices: when I cons a new bound variable onto the local environment, it automatically has index 0, and all the other local bindings are automatically shifted up one.
data Env = Env { globalEnv :: M.Map String (AST.Typ, AST.Exp)
               , localEnv :: [AST.Typ]
               }

newEnv :: (M.Map String (AST.Typ, AST.Exp)) -> Env
newEnv globalEnv = Env {globalEnv, localEnv = []}

addGlobalEnv :: String -> (AST.Typ, AST.Exp) -> Env -> Env
addGlobalEnv key v env = env {globalEnv = M.insert key v (globalEnv env)}

emptyEnv :: Env
emptyEnv = Env M.empty []

