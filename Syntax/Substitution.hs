module Syntax.Substitution where

import qualified Data.Set as Set

import Types.Term
import Syntax.Variables

substitute :: Term -> Var -> Term -> Term

substitute (VarTerm y) x replacement =
    if y == x
    then replacement
    else (VarTerm y)

substitute (Combination left right) x replacement = Combination newLeft newRight
    where newLeft = substitute left x replacement
          newRight = substitute right x replacement

substitute (Abstraction y term) x replacement
    | y == x =
        (Abstraction y term)

    | Set.member y repVars && Set.member x termVars =
        Abstraction newVar (substitute renamed x replacement)

    | otherwise =
        Abstraction y (substitute term x replacement)

  where
    termVars = freeVars term
    repVars = freeVars replacement
    newVar = succ $ Set.findMax repVars
    renamed = substitute term y (VarTerm newVar)
