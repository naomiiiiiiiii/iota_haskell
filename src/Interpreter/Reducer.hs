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
  Binop (op, expr1, expr2) ->
    do
      v1 <- reduce expr1
      v2 <- reduce expr2
      return $ semanticOp op v1 v2
  Ap(fn, arg) ->
    do
      fnVal <- reduce fn
      argVal <- reduce arg
      case fnVal of
        Lam(_, body) -> reduce (subst 0 argVal body)
        _  -> return $ Ap (fnVal, argVal)
  Bind(exp0, body) -> do
    val0Suspended <- reduce exp0
    val0 <- unsuspend val0Suspended -- release the computation in @exp0@
    reduce (subst 0 val0 (snd body)) --  evaluate @body@ (with the bound variable replaced by @val0@)
  -- @unsuspend comp@ expects @comp@ to be a fully-reduced computation (ie, an expression with @ret@, @ref@, @:=@, or @!@ at the top). It releases and runs the suspended computation @comp@.
  where
    unsuspend :: AST.Exp -> StateT (M.IntMap AST.Exp) (Reader Env.Env) AST.Exp
    unsuspend comp = case comp of
     Ret exp0 -> reduce exp0

-- Given an AST binop, return the @Exp -> Exp -> Exp@ function that does what the @Bop@ means
semanticOp :: Bop -> Exp -> Exp -> Exp
semanticOp Plus e1 e2  =
  let errStr = "expected integer argument to (+), recieved " in
    case (e1, e2) of
      (Int v1, Int v2) -> Int (v1 + v2)
      (Int _, _) -> rError (errStr ++ (show e2))
      _ -> rError (errStr ++ (show e1))

------------
--- Pretty printing for the store
------------
instance (Pretty a) => Pretty (M.IntMap a) where
  pretty store = braces $ concatWith (\x y -> x <> "," <+> y)
                 (map (\(k, v) -> parens $ ("Loc:" <+> (pretty k) <> "," <+>
                                          "Contents:" <+> (pretty v)))
                   (M.assocs store))
