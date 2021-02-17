module Test.Parser where

import Prelude

import Data.Either (Either(..))
import Formula (Formula(..))
import Parser (parseFormula)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do simpleParserTest
          testConjunction -- 

simpleParserTest :: Spec Unit
simpleParserTest = describe "Formula parser" do
  it "should work" do
    (parseFormula "¬(A ∧ B)") `shouldEqual`
      Right (Not (And
                  (Predicate "A" [])
                  (Predicate "B" [])))
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