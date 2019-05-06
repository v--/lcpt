-- Exercise 2.7. Implement lambda-term substitution which renames variables when appropriate

import Test.HUnit

import Types.Term
import Syntax.Substitution

x = Var 'x'
y = Var 'y'
z = Var 'z'
t = Var 't'

rep = readTerm "(λxt)"

main = runTestTT $ test [
    "Substitution replaces a single variable" ~:
      let m = readTerm "x" in
        substitute m x rep ~?= rep,

    "Substitution does not replace a single non-matching variable" ~:
      let m = readTerm "y" in
        substitute m x rep ~?= m,

    "Substitution replaces variables in a combination's subterms" ~:
      let m = readTerm "(xx)" in
        substitute m x rep ~?= Combination rep rep,

    "Substitution does not replace bound variables in an abstraction's subterm" ~:
      let m = readTerm "(λxx)" in
        substitute m x rep ~?= m,

    "Substitution replaces free variables in an abstraction's subterm" ~:
      let m = Abstraction z (VarTerm x) in
        substitute m x rep ~?= Abstraction z rep,

    "Substitution renames problematic bound variables in an abstraction's subterm" ~:
      let m = readTerm "(λtx)" in
        substitute m x rep ~?= readTerm "(λy(λxt))",

    "Substitution renames bound variables that collide with any of the new variables" ~:
      let m = readTerm "(λt(λy((xy)(ts))))" in
        substitute m x (readTerm "t") ~?= readTerm "(λy(λz((tz)(ys))))",

    "Substitution does not rename nested bound variables if those variables are unused" ~:
      let m = readTerm "(λy(λtx))" in
        substitute m x rep ~?= readTerm "(λy(λy(λxt)))"
  ]
