-- The AST for the Iota language and helper functions
module AST (Typ(..) , Exp(..))
where

-- TODO: Add strings?
data Typ = IntTyp
         | UnitTyp
         | ArrowTyp (Typ, Typ) -- function
         | RefTyp Typ -- reference
         | CompTyp Typ -- suspended stateful computation, written ○

data Exp = Free String
         | Bound Int
         | Star -- @Star@ is the only value of type @Unit@
         | Int Int
         | Loc Int -- location into the store. This is an internal expression, not accessible to the user.
         | Plus (Exp, Exp)
         | Lam ((String, Typ),  Exp)
         | Ap (Exp, Exp)
         | Ret Exp
         | Bind (Exp, (String, Exp))
         | Ref Exp
         | Asgn (Exp, Exp)
         | Deref Exp
