module Test.Parser where

import Prelude

import Formula (Variable(..), Term(..), Formula(..))
import Data.Either (Either(..))
import Parser (parseFormula)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do simpleParserTest
          testConjunction -- 

simpleParserTest :: Spec Unit
simpleParserTest = describe "Formula parser" do
  it "should work" do
    parseFormula "¬(A ∧ B)" `shouldEqual`
      Right (Not (And
                  (Predicate "A" [])
                  (Predicate "B" [])))
  it "should handle whitespace" do
    parseFormula " ∃ x P ( x ) → ( A ) " `shouldEqual`
      Right (Implies
             (Exists (Variable "x") (Predicate "P" [(Var $ Variable "x")]))
             (Predicate "A" []))
  it "parses constants and variables" do
    parseFormula "∀x Le(x, c())" `shouldEqual`
      Right (Forall (Variable "x")
            (Predicate "Le" [Var $ Variable "x", App "c" []]))
  it "parses the equality predicate" do
    parseFormula "=(x, y)" `shouldEqual`
      Right (Predicate "=" [Var $ Variable "x", Var $ Variable "y"])
    parseFormula "x = y" `shouldEqual`
      Right (Predicate "=" [Var $ Variable "x", Var $ Variable "y"])
  it "should parse double negation" do
    parseFormula "¬¬P" `shouldEqual` Right (Not (Not (Predicate "P" [])))
  it "does not care about 'left' parentheses" do
    (parseFormula "(A ∧ B) ∧ C") `shouldEqual`
      Right (And 
              (And 
                (Predicate "A" [])
                (Predicate "B" []))
              (Predicate "C" []))
  it "cares right parentheses" do
    (parseFormula "A ∧ (B ∧ C)") `shouldEqual`
      Right (And
              (Predicate "A" [])
              (And
                (Predicate "B" [])
                (Predicate "C" [])))
  it "properly parses implication" do
    (parseFormula "(A ∧ B) → (B ∧ A)") `shouldEqual`
      Right (Implies
              (And
                (Predicate "A" [])
                (Predicate "B" []))
              (And
                (Predicate "B" [])
                (Predicate "A" [])))

testConjunction :: Spec Unit
testConjunction = describe "Conjunction parser tests" do
  it "is left associative" do
    (parseFormula "A ∧ B ∧ C") `shouldEqual`
      Right (And 
              (And 
                (Predicate "A" [])
                (Predicate "B" []))
              (Predicate "C" []))
