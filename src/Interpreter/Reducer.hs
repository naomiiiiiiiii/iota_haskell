-- Reduce Iota expressions to values

module Interpreter.Reducer (reduce)
  where

import qualified AST as AST
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M

-- @reduce@ takes an expression and reduces it to a value
-- While doing this, it reads from the environment of global typed expression declarations. This environment is contained in the reader.
reduce :: AST.Exp -> ReaderT (M.Map String (AST.Typ, AST.Exp)) (State (M.Map Int AST.Exp)) AST.Exp
reduce = return
