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
    Binop(bop, exp1, exp2) -> do
      tau1 <- typeChecker exp1
      tau2 <- typeChecker exp2
      let (bopArg1, bopArg2, bopRet) = bopTyp bop -- get the argument types that @bop@ expects
      case (bopArg1 == tau1, bopArg2 == tau2) of
        (True, True) -> return bopRet -- The provided arguments have the expected types
        (False, _) -> tcError ("expected " ++ (show bopArg1) ++
                                  " , recieved " ++ (showTyping exp1 tau1)) -- First arg is wrong
        (_, False)-> tcError ("expected " ++ (show bopArg2) ++
                                  " , recieved " ++ (showTyping exp2 tau2)) -- Second arg is wrong
    Lam ((_, tau0), body) -> local (Env.addLocalEnv tau0)
                             ((curry ArrowTyp) tau0 <$> typeChecker body)
    Ap(fn, arg) -> do
      tauFn <- typeChecker fn
      tauArgReal <- typeChecker arg
      case tauFn of
        ArrowTyp(tauArg, tauOut) | (tauArg == tauArgReal) -> return tauOut
        _ -> tcError ("cannot apply " ++ (show tauFn) ++ " to "
                      ++ (showTyping arg tauArgReal))
    Ret(exp0) -> CompTyp <$> typeChecker exp0
    -- G |- exp0 : Comp A and G,A |- body : Comp B means G |- bind(exp0, body) : Comp B
    Bind(exp0, ( _, body)) -> do
      tauComp <- typeChecker exp0
      case tauComp of
        CompTyp tauBound -> do
          tauOut <- local (Env.addLocalEnv tauBound) (typeChecker body)
          case tauOut of
            CompTyp _ -> return tauOut
            _ -> tcError ("expected bind to return a computation, instead got " ++ (showTyping body tauOut))
        _ -> tcError ("cannot bind " ++ (showTyping exp0 tauComp))

    Ref(exp0) -> CompTyp <$> RefTyp <$> typeChecker exp0  -- a reference is a delayed computation which, when run, stores @m0@ and then evaluates to a location
    -- Assignments return nothing after modifying the store
    -- G |- loc : Ref A and G |- rhs : A means G |- loc := rhs : Comp Unit
    Asgn(loc, rhs) -> do
       tauLoc <- typeChecker loc
       tauRHS <- typeChecker rhs
       case tauLoc of
         RefTyp tauContents | (tauContents == tauRHS) -> return $ CompTyp UnitTyp
         _ -> tcError ("cannot assign " ++ (showTyping rhs tauRHS) ++ " to ref of type " ++ (show tauLoc))
    -- G |- loc : Ref A means G |- !loc : Comp A
    Deref loc -> do
      tauLoc <- typeChecker loc
      case tauLoc of
       RefTyp tauContents -> return $ CompTyp tauContents
       _ -> tcError ("cannot dereference " ++ (showTyping loc tauLoc))
  where
    -- Given a binop, @bopTyp@ returns (in order) the first argument type, the second argument type, and the return type
    bopTyp :: Bop -> (Typ, Typ, Typ)
    bopTyp bop = case bop of
      Plus -> (IntTyp, IntTyp, IntTyp)
    showTyping :: Exp -> Typ -> String
    showTyping expr0 typ = show $ prettyTyping expr0 typ
