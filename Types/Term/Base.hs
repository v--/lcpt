module Types.Term.Base where

import Types.Var

data Term = VarTerm { var :: Var } |
            Combination { left :: Term, right :: Term } |
            Abstraction { var :: Var, term :: Term }
