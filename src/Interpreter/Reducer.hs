{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- Reduce Iota expressions to values

module Interpreter.Reducer (reduce)
  where

import AST as AST
import qualified Interpreter.Environment as Env
import Control.Monad.Reader
import Control.Monad.State.Strict -- ensures that Iota has strict semantics
import qualified Data.IntMap as M
import Prettyprinter

------------
--- Error handling
------------

-- throw an error, making it clear that it comes from the Reducer file
-- Because of typechecking, this should never be called!
rError :: String -> a
rError msg = error ("RUNTIME ERROR (Reducer.reduce) " ++ msg)

------------
--- Evaluate expressions
------------

-- @reduce@ takes an expression and reduces it to a value
-- While doing this, it reads from the environment of global typed expression declarations. This environment is contained in the reader.
reduce :: AST.Exp -> ReaderT (M.Map String (AST.Typ, AST.Exp)) (State (M.Map Int AST.Exp)) AST.Exp
reduce = return
