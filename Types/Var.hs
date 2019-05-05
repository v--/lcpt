module Types.Var where

newtype Var = Var { varName :: Char }

instance Eq Var where
    (Var aName) == (Var bName) = aName == bName

instance Show Var where
    show (Var name) = name:""

instance Read Var where
    readsPrec _ (h:"") = [((Var h), "")]
    readsPrec _ _ = []

instance Ord Var where
    compare (Var aName) (Var bName) = compare aName bName

instance Enum Var where
    fromEnum (Var name) = fromEnum name
    toEnum value = Var $ toEnum value
