module Test.Formula where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Formula (Term(..), Variable(..), Formula(..), singleSub, substitute)
import Data.Foldable (fold)
import Data.Maybe (fromJust)
import Data.Traversable (sequence)
import Partial.Unsafe (unsafePartial)

spec :: Spec Unit
spec = describe "Formulas" do
  substitutionTests
  where
    substitutionTests = describe "substitutions" do
      it "can do substitution on formula" do
        let θ = fold $ unsafePartial $ fromJust $ sequence
                [ singleSub (Variable "x") (App "a" [])
                , singleSub (Variable "y") (App "f" [App "b" []])
                , singleSub (Variable "z") (App "c" []) ]
            e = Predicate "P" [Var (Variable "x"), Var (Variable "y"), Var (Variable "z")]
        substitute θ e `shouldEqual`
          Predicate "P" [App "a" [], App "f" [App "b" []], App "c" []]
