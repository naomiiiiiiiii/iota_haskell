-- The AST for the Iota language and helper functions
module AST (Typ(..) , Exp(..), absList, applyList, plus, bind)
where

-- TODO: Add strings?
data Typ = IntTyp
         | UnitTyp
         | ArrowTyp (Typ, Typ) -- function
         | RefTyp Typ -- reference, written "Ref([Typ])"
         | CompTyp Typ -- suspended stateful computation, written "Comp([Typ])"
  deriving(Show, Eq)

data Exp = Free String
         | Bound Int -- This AST uses De Bruijn indices
         | Star -- @Star@ is the only value of type @Unit@, written "()"
         | Int Int
         | Loc Int -- location into the store. This is an internal expression, not accessible to the user.
         | Binop (Bop, Exp, Exp)
         | Lam ((String, Typ),  Exp) -- lambdas are written "\(x : Int).x", for example
         | Ap (Exp, Exp) -- function application is written "f x y" or "f(x y)"
         | Ret Exp -- return into the state monad, written "ret [Exp]"
         | Bind (Exp, (String, Exp)) -- bind operation for the state monad, written "bind([Exp], \[Name].[Exp])"
         | Ref Exp -- a reference to an expression, written "ref [Exp]"
         | Asgn (Exp, Exp) -- assign to a reference, written "[Exp] := [Exp]"
         | Deref Exp -- dereference a reference, written "![Exp]"
  deriving(Show, Eq)

-- TODO: Add more binops
data Bop = Plus
  deriving(Show, Eq)

------------
--- Functions for traversing over the AST
------------

-- At a high level, @mapExpWDepth@ is like @mapi :: (Int -> a -> b) -> [a] -> [b]@, but for expressions
-- @mapi@ maps a function over each element of the argument list to get a new list. That function takes an element of the list, of course, but it also takes that element's index.
-- In a similar way, @mapExpWDepth@ applies a function to each variable in the argument expression. That function has two components -- one component takes a string argument to handle free variables, the other component takes an integer argument to handle bound variables. Both function components take a second argument: the variable's depth. In the context of an expression, a variable's depth is how many binders deep that variable is. For instance, the variable @x@ in expression @x + 5@ is 0 binders deep. But the expression @x@ in @\(y x).x@ is two binders deep.
-- Variable depth in @mapExpWDepth@ is analagous to element index in @mapi@.
-- So @mapExpWDepth varCase startingIndex exp@ applies the appropriate function in @varCase@ to all the variables in @exp@. -- START HERE refactor to remove @start@, unnecessarily complicated.
mapExpWDepth :: (Int -> (String -> Exp, Int -> Exp)) -> Int -> Exp -> Exp
mapExpWDepth varCase start expr =
    let mapExpWDepthHelp = mapExpWDepth varCase
        varCases = varCase start in
    case expr of
    Free str -> (fst varCases) str
    Bound i -> (snd varCases) i
    Star -> expr
    Int _ -> expr
    Loc _ -> expr
    Binop (op, exp1, exp2) -> Binop (op, mapExpWDepthHelp start exp1, mapExpWDepthHelp start exp2)
    Lam (arg, body) -> Lam(arg, (mapExpWDepthHelp (start + 1)) body)
    Ap(exp1, exp2) -> Ap((mapExpWDepthHelp start exp1), (mapExpWDepthHelp start exp2))
    Ret(exp0) -> Ret(mapExpWDepthHelp start exp0)
    Bind(exp1, (s, exp2)) -> Bind((mapExpWDepthHelp start exp1), (s, (mapExpWDepthHelp (start + 1) exp2)))
    Ref(exp0) -> Ref(mapExpWDepthHelp start exp0)
    Asgn(ref, exp0) -> Asgn((mapExpWDepthHelp start ref), (mapExpWDepthHelp start exp0))
    Deref ref -> Deref(mapExpWDepthHelp start ref)

------------
--- Functions for manipulating AST variables
------------
-- For variable index i >= 0, @abstract i var exp@ will turn all free occurences of variable @var@ in @exp@ into occurences of the bound variable indexed by @i@
-- This function is useful when constructing a lambda abstraction. The lambda body starts out with only free variables. But, as each binder is added over the body expression, the corresponding free variable in the body is captured by the binder and converted to a bound variable
abstract :: Int -> String -> Exp -> Exp
abstract i boundVarStr = let varCase = \j -> ( let freeVarHandler = \varStr ->
                                                               if (varStr == boundVarStr)
                                                               then (Bound j)
                                                               else (Free varStr) -- Free variables not named @boundVarStr@ are left alone
                                                in (freeVarHandler, Bound))
                         in mapExpWDepth varCase i

-- @shift i threshold exp@ shifts @exp@'s bound variables up by @i@, while ignoring variables <= @dot@*)
shift :: Int -> Int -> Exp -> Exp
shift i threshold =
  let varCase = \varDepth -> (
        let boundCase = \j -> if j>=(varDepth + threshold)
                              then Bound(j+i)
                              else Bound j
        in (Free, boundCase))
  in mapExpWDepth varCase 0

-- For variable index @i@, @subst i v expr@ equals expr[Bound i := v], which is to say
-- @expr@ with all occurences of variable @Bound i@ replaced by @v@
-- START HERE refactored this so if there's a bug look here
subst :: Int -> Exp -> Exp -> Exp
subst i v =
  let varCase = \varDepth -> (
                  let boundCase = \j ->
                        -- Why compare @j@ to the desired variable @i@ plus the variable depth? When going under a lambda, all the variable indexes shift up, including @i@. For example, (Bound 0 + \x.Bound 1)[0:= Int 5] would be (5 + \x. 5)
                        case (compare j (i + varDepth)) of
                          LT ->  Bound j -- @j@ is unaffected by the substitution
                          EQ -> shift varDepth 0 v -- @j@ is the desired variable, so we need to substitute. We shift @v@ up @varDepth@ to avoid @v@ capturing any local variables
                          GT -> Bound(j-1) -- @(i + varDepth)@ has been removed, so @j@ moves leftwards in the lambda stack
                  in (Free, boundCase ))
  in mapExpWDepth varCase 0

------------
--- Functions for constructing ASTs
------------

-- @absList args body@ creates a lambda abstraction with arguments @args@ (by name and type) and body @body@
-- For each argument in @args@, @absList@ binds all free occurences of that argument in @body@ to the right bound variable, based on the order of @args@.
absList :: ([(String, Typ)], Exp) -> Exp
absList (varList, body) = foldr (\typedVar bodyAcc -> Lam(typedVar, (abstract 0 (fst typedVar) bodyAcc))) body varList

-- @applyList fn args@ creates the application of @fn@ to @args@
applyList :: (Exp, [Exp]) -> Exp
applyList (fn, args) = foldl (\bigApp arg -> Ap(bigApp, arg)) fn args

plus :: (Exp, Exp) -> Exp
plus (e1, e2) = Binop(Plus, e1, e2)

bind :: (Exp, (String, Exp)) -> Exp
bind (exp0, (name, exp1)) = Bind(exp0, (name, (abstract 0 name exp1)))
