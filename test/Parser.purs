module Test.Parser where

import Prelude
import Data.Either (Either(..), isLeft)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Formula (Variable(..), Term(..), Formula(..), bottomProp)
import Parser (parseFormula)

spec :: Spec Unit
spec =
  describe "Formula parser" do
    it "should work" do
      parseFormula "¬(A ∧ B)"
        `shouldEqual`
          Right
            ( Not
                ( And
                    (Predicate "A" [])
                    (Predicate "B" [])
                )
            )
    it "should handle whitespace" do
      parseFormula " ∃ x P ( x ) → ( A ) "
        `shouldEqual`
          Right
            ( Implies
                (Exists (Variable "x") (Predicate "P" [ (Var $ Variable "x") ]))
                (Predicate "A" [])
            )
    it "parses constants and variables" do
      parseFormula "∀x Le(x, c())"
        `shouldEqual`
          Right
            ( Forall (Variable "x")
                (Predicate "Le" [ Var $ Variable "x", App "c" [] ])
            )
    it "parses the equality predicate" do
      parseFormula "=(x, y)"
        `shouldEqual`
          Right (Predicate "=" [ Var $ Variable "x", Var $ Variable "y" ])
      parseFormula "x = y"
        `shouldEqual`
          Right (Predicate "=" [ Var $ Variable "x", Var $ Variable "y" ])
    it "should parse double negation" do
      parseFormula "¬¬P" `shouldEqual` Right (Not (Not (Predicate "P" [])))
    it "can parse the bottom symbol" do
      parseFormula "⊥" `shouldEqual` Right bottomProp
    it "does not care about redundant parentheses" do
      parseFormula "(A ∧ B) ∧ C"
        `shouldEqual`
          Right
            ( And
                ( And
                    (Predicate "A" [])
                    (Predicate "B" [])
                )
                (Predicate "C" [])
            )
    it "can decide correct operator precedence from parentheses"
      $ parseFormula "A ∧ (B ∧ C)"
          `shouldEqual`
            Right
              ( And
                  (Predicate "A" [])
                  ( And
                      (Predicate "B" [])
                      (Predicate "C" [])
                  )
              )
    it "properly parses implication" do
      parseFormula "(A ∧ B) → (B ∧ A)"
        `shouldEqual`
          Right
            ( Implies
                ( And
                    (Predicate "A" [])
                    (Predicate "B" [])
                )
                ( And
                    (Predicate "B" [])
                    (Predicate "A" [])
                )
            )
    it "parses left associative formula correctly"
      $ parseFormula "A ∧ B ∧ C"
          `shouldEqual`
            Right
              ( And
                  ( And
                      (Predicate "A" [])
                      (Predicate "B" [])
                  )
                  (Predicate "C" [])
              )
    it "allows digits in variable names"
      $ parseFormula "P(x0)" `shouldEqual` Right (Predicate "P" [ Var $ Variable "x0" ])
    it "does not allow variables/predicates to start with a digit" do
      parseFormula "P(0x)" `shouldSatisfy` isLeft
      parseFormula "0A" `shouldSatisfy` isLeft
    it "can parse quantified formula without space"
      $ parseFormula "∀xP(x)" `shouldEqual` Right (Forall (Variable "x") $ Predicate "P" [ Var $ Variable "x" ])
    it "parses lowercase propositions"
      $ parseFormula "p" `shouldEqual` Right (Predicate "p" [])
    it "does not allow proposition in quantified formula"
      $ parseFormula "∃xp(x)" `shouldSatisfy` isLeft
