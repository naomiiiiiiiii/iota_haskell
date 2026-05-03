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
  Bound _ -> rError ("Cannot reduce bound variable: " ++ (show expr))
  -- @unsuspend comp@ expects @comp@ to be a fully-reduced computation (ie, an expression with @ret@, @ref@, @:=@, or @!@ at the top). It releases and runs the suspended computation @comp@.
  where
    unsuspend :: AST.Exp -> StateT (M.IntMap AST.Exp) (Reader Env.Env) AST.Exp
    unsuspend comp = case comp of
     Ret exp0 -> reduce exp0
     Ref exp0 -> do
       val0 <- reduce exp0
       -- Add @val0@ to the store
       nextIndex <- getNextKey
       modify (\store -> M.insert nextIndex val0 store)
       return $ Loc nextIndex
     Asgn(loc, rhs) -> do
       index <- getIndex "cannot assign to non-location: " <$> reduce loc
       rhsVal <- reduce rhs
       let errStr = "assigning to uninitialized location: " ++ (show loc)
       -- Update location @index@ to store @rhsVal@
       modify (\store -> M.alter (\case
                                     Just _ -> Just rhsVal
                                     Nothing -> rError errStr)
                         index
                         store)
       return Star -- Assignments don't return a meaningful value, they only update the store
     Deref loc -> do
       index <- getIndex "cannot dereference non-location: " <$> reduce loc
       store <- get
       case M.lookup index store of
         Just out -> return out
         Nothing -> rError ("dereferencing uninitialized location: " ++ (show loc))
     _ -> rError ("expected computation, got " ++ (show comp))
    -- @getIndex@ gets the integer index out of the provided expression
    -- if the provided expression is not a raw location, @getIndex@ fails
    getIndex :: String -> Exp -> Int
    getIndex errStr loc = case loc of
                            Loc index -> index
                            _ -> rError (errStr ++ (show loc))

-- @getNextKey@ returns the next available index into the store
-- TODO: This is a little slow, would be faster if you stored next key in the state
    getNextKey :: (Monad b) => StateT (M.IntMap a) b Int
    getNextKey = do
      store <- get
      return $ case M.lookupMax store of
                 Just (maxKey, _) -> maxKey + 1
                 Nothing -> 0 -- store is currently empty

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
