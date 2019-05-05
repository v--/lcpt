module Types.Term.Parse where

import qualified Data.Maybe as Maybe

import Types.Var
import Types.Term.Base

-- Produce all possible splittings of word into two non-empty strings
substringPairs :: String -> [(String, String)]
substringPairs string = map (`splitAt` string) [1..length string - 1]

-- Try to parse a pair of term strings
parseTermPair :: (String, String) -> Maybe (Term, Term)
parseTermPair (leftString, rightString) = do
    leftTerm <- parseTerm leftString
    rightTerm <- parseTerm rightString
    Just (leftTerm, rightTerm)

-- Try to parse a combination or abstraction with removed parentheses
parseUnwrappedTerm :: String -> Maybe Term

-- Try to parse an abstraction
parseUnwrappedTerm ('λ':varName:subtermString) = do
    subterm <- parseTerm subtermString
    Just $ Abstraction (Var varName) subterm

-- Try to parse a combination
parseUnwrappedTerm string =
    if null matches
    then Nothing
    else Just (Combination (fst firstMatch) (snd firstMatch))
  where matches = Maybe.mapMaybe parseTermPair $ substringPairs string
        firstMatch = head matches

parseTerm :: String -> Maybe Term
parseTerm "" = Nothing
parseTerm (varName:"") = Just (VarTerm (Var varName))
parseTerm string =
    if head string == '(' && last string == ')'
    then parseUnwrappedTerm (init . tail $ string)
    else Nothing
