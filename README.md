# Lambda calculus and proof theory

This repository contains some of the exercises for an introductory course for lambda calculus with elements of proof theory.

Most of the exercises are propositions for proving and so this repository contains only the fraction of exercises that require implementations.

All exercises are verified with unit tests that can be ran using `make test=exercises/2.1.hs` or, in order to run all the tests, simply `make test`.

# Terms

The term implementation in the `types/term` directory contains a parser and a serializer for terms. Haskell's `read` and `show` functions can also be used. Here are some specificities of the representation:

* All valid Char value are valid variable names. Even `read "λ" :: Var` creates a variable named `λ`.
* Variables are ordered and enumerable, so comparison and `succ` are valid operations on variables. This is useful for creating new variables for use in substitution.
* `Term` types are sum types of `VarTerm`, `Combination` or `Abstraction` types. The string representations are:
    * Variable names for `VerTerm` values.
    * Concatenated terms in parentheses for `Combination` values (e.g. `(xy)`)
    * A `λ` character concatenated with a variable name and a term, put in parentheses for `Abstraction` values (e.g. `(λxy)` or even `(λλλ)`)
