-- Exercise 2.1. Implement a program that checks whether a word is a lambda-term

import Test.HUnit

import Types.Term
import Types.Term.Parse

assertValidTermString :: String -> String -> Assertion
assertValidTermString msg string = Nothing /= parseTerm string @? msg

assertInvalidTermString :: String -> String -> Assertion
assertInvalidTermString msg string = Nothing == parseTerm string @? msg

main = runTestTT $ test [
    "The parser recognizes" ~: test [
      assertValidTermString "single variables" $ "x",
      assertValidTermString "the identity"     $ "(λxx)",
      assertValidTermString "the K combinator" $ "(λx(λyx))",
      assertValidTermString "the S combinator" $ "(λx(λy(λz((xz)(yz)))))",
      assertValidTermString "the Y combinator" $ "(λf((λx(f(xx)))(λx(f(xx)))))",
      assertValidTermString "the ω combinator" $ "(λx(xx))"
    ],

    "The parser fails to recognize" ~: test [
      assertInvalidTermString "the empty string"                 $ "",
      assertInvalidTermString "applications without parentheses" $ "xx",
      assertInvalidTermString "combinations without parentheses" $ "λxx",
      assertInvalidTermString "gibberish"                        $ "gibberish"
    ]
  ]
