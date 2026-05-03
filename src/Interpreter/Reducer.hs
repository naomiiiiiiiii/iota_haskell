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
-- It also modifies and reads from the global memory store. This is contained in the state.
reduce :: AST.Exp -> StateT (M.IntMap AST.Exp) (Reader Env.Env) AST.Exp
reduce expr = case expr of
  Free ident -> (snd <$> (lift $ Env.lookUpGlobalU ident)) >>= reduce
  Star -> return expr -- Value, cannot be reduced
  Int _  -> return expr -- Value
  Loc _ -> return expr -- Value
  Lam _ -> return expr -- Value
  Ret _ -> return expr -- Suspended computation
  Ref _ -> return expr -- Suspended computation
  Asgn _ -> return expr -- Suspended computation
  Deref _ -> return expr  -- Suspended computation
