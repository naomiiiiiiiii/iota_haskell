-- The AST for the Iota language and helper functions
module AST (Typ(..) , Exp(..))
where

-- TODO: Add strings?
data Typ = IntTyp
         | UnitTyp
         | ArrowTyp (Typ, Typ) -- function
         | RefTyp Typ -- reference, written "Ref([Typ])"
         | CompTyp Typ -- suspended stateful computation, written "Comp([Typ])"

data Exp = Free String
         | Bound Int
         | Star -- @Star@ is the only value of type @Unit@, written "()"
         | Int Int
         | Loc Int -- location into the store. This is an internal expression, not accessible to the user.
         | Plus (Exp, Exp) -- Addition on integer expressions, written "x + y"
         | Lam ((String, Typ),  Exp) -- lambdas are written "\(x : Int).x", for example
         | Ap (Exp, Exp) -- function application is written "f x y" or "f(x y)"
         | Ret Exp -- return into the state monad, written "ret [Exp]"
         | Bind (Exp, (String, Exp)) -- bind operation for the state monad, written "bind([Exp], \[Name].[Exp])"
         | Ref Exp -- a reference to an expression, written "ref [Exp]"
         | Asgn (Exp, Exp) -- assign to a reference, written "[Exp] := [Exp]"
         | Deref Exp -- dereference a reference, written "![Exp]"
