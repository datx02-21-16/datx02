module Test.Parser where

import Prelude
import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Formula (Formula(..))
import Parser (parseFormula)

spec :: Spec Unit
spec = describe "Formula parser" do
  it "should work" do
    (parseFormula "¬(A ∧ B)") `shouldEqual`
      Right (Not (And
                  (Predicate "A" [])
                  (Predicate "B" [])))
