-- A typechecker for the Iota language
module Interpreter.Typechecker (typeChecker)
  where

import AST
import Interpreter.Environment as Env

import Control.Monad.Reader

------------
--- Error handling
------------
-- TODO: Improve error handling, have typeChecker return an @Either@

-- throw an error, making it clear that it comes from the Typechecker file
tcError :: String -> a
tcError msg = error ("Typechecker.typechecker" ++ msg)

------------
--- Typechecker
------------
-- The typechecker takes an expression and returns its type
-- While doing this, it reads from the environment of global typed expression declarations. This environment is contained in the reader.
-- @typeChecker@ also reads from and extends the environment of locally bound variable types (contained in the reader as well). Because this environment is only ever extended locally (when going under a binder), I think it is appropriate to use a @Reader@, rather than @State@, for this environment as well
typeChecker :: AST.Exp -> Reader Env.Env AST.Typ
typeChecker expr = case expr of
    Free var -> fst <$> Env.lookUpGlobalU var
    Bound i -> Env.lookUpLocalU i
    Star -> return UnitTyp
    Int _ -> return IntTyp
    Loc i -> tcError ("uninitialized location: " ++ (show i))
    --raw locations should only show up during execution, not when a program is typechecked
