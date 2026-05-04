{-# LANGUAGE NamedFieldPuns#-}

-- An environment of global and local Iota expression bindings, useful in typechecking and d expression reduction
-- Also, the global mutable memory store, used in expression reduction
module Interpreter.Environment (Env(..), Store(..)
                              , addGlobalEnv, addLocalEnv
                              , lookUpGlobalU, lookUpLocalU
                              , emptyEnv)
  where

import qualified AST as AST
import qualified Data.Map as M
import qualified Data.IntMap as IntM
import Data.List ((!?))
import Control.Monad.Reader

------------
--- Global and local environments
------------
-- When interpretting an expression, we have recourse to two environments
-- The first is all previous global expression declarations, represented as a map from strings (variable names) to typed expressions.
-- The second is all local bindings (introduced by lambdas), represented as a simple list of types. Lists work nicely with local bindings' De Bruijn indices: when I cons a new bound variable onto the local environment, it automatically has index 0, and all the other local bindings are automatically shifted up one.
data Env = Env { globalEnv :: M.Map String (AST.Typ, AST.Exp)
               , localEnv :: [AST.Typ]
               }

addGlobalEnv :: String -> (AST.Typ, AST.Exp) -> Env -> Env
addGlobalEnv key v env = env {globalEnv = M.insert key v (globalEnv env)}

addLocalEnv :: AST.Typ -> Env -> Env
addLocalEnv ty env = env {localEnv = ty:(localEnv env)}

lookUpGlobal :: String -> Reader Env (Maybe (AST.Typ, AST.Exp))
lookUpGlobal key = do
  Env global _ <- ask
  return $ M.lookup key global

lookUpLocal :: Int -> Reader Env (Maybe AST.Typ)
lookUpLocal key = do
  Env _ localEnv <- ask
  return $ localEnv !? key

lookUpGlobalU :: String -> Reader Env (AST.Typ, AST.Exp)
lookUpGlobalU = lookUpU lookUpGlobal "lookUpGlobalU" "identifier"

lookUpLocalU :: Int -> Reader Env AST.Typ
lookUpLocalU = lookUpU lookUpLocal "lookUpLocalU" "variable"

emptyEnv :: Env
emptyEnv = Env M.empty []

-- unsafe lookup
lookUpU :: (Monad m, Show a) => (a -> m (Maybe b)) -> [Char] -> [Char] -> a -> m b
lookUpU lookUpFn fnName errorName key= do
  outMaybe <- lookUpFn key
  case outMaybe of
    Just out -> return out
    Nothing -> error ("Environment." ++ fnName ++ ": "
                      ++ "Unbound " ++ errorName ++ " " ++ (show key))

------------
--- Mutable memory store
------------

data Store = Store {storeMap :: IntM.IntMap AST.Exp -- map of locations to the expressions stored therein
                  , nextIndex :: Int -- next available location
                  }
