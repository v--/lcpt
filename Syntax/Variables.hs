module Syntax.Variables where

import qualified Data.Set as Set

import Types.Term

freeVars :: Term -> Set.Set Var
freeVars (VarTerm var) = Set.singleton var
freeVars (Combination left right) = freeVars left `Set.union` freeVars right
freeVars (Abstraction var term) = Set.delete var $ freeVars term

boundVars :: Term -> Set.Set Var
boundVars (VarTerm var) = Set.empty
boundVars (Combination left right) = boundVars left `Set.union` boundVars right
boundVars (Abstraction var term) = Set.singleton var `Set.union`  freeVars term
