module Types.Term (module Types.Var, module Types.Term.Base, module Types.Term) where

import qualified Data.Maybe as Maybe

import Types.Var
import Types.Term.Base
import Types.Term.Parse

instance Eq Term where
    (VarTerm aVar) == (VarTerm bVar) = aVar == bVar
    (Combination aLeft aRight) == (Combination bLeft bRight) = aLeft == bLeft && aRight == bRight
    (Abstraction aVar aTerm) == (Abstraction bVar bTerm) = aVar == bVar && aTerm == bTerm
    (_) == (_) = False

instance Show Term where
    show (VarTerm var) = show var
    show (Combination left right) = "(" ++ show left ++ show right ++ ")"
    show (Abstraction var term) = "(λ" ++ show var ++ show term ++ ")"

instance Read Term where
    readsPrec _ string = if Maybe.isNothing term then [] else [(Maybe.fromJust term, "")]
        where term = parseTerm string

-- An unsafe term constructor
readTerm :: String -> Term
readTerm string = Maybe.fromJust . parseTerm $ string
