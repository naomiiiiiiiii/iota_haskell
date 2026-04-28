-- A typechecker for the Iota language
module Interpreter.Typechecker (typeChecker, Env(..), newEnv, addGlobalEnv, emptyEnv)
  where

import qualified AST as AST
import Interpreter.Environment as Env

import Control.Monad.Reader

-- The typechecker takes an expression and returns its type
-- While doing this, it reads from the environment of global typed expression declarations. This environment is contained in the reader.
-- @typeChecker@ also reads from and extends the environment of locally bound variable types (contained in the reader as well). Because this environment is only ever extended locally (when going under a binder), I think it is appropriate to use a @Reader@, rather than @State@, for this environment as well
typeChecker :: AST.Exp -> Reader Env.Env AST.Typ
typeChecker expr = error "TODO"
