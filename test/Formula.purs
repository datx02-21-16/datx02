module Test.Formula where


import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Data.List as List
import Data.List (List(Nil, Cons))
import Data.Set as Set
import Formula (Term(..), Variable(..), Formula(..), singleSub, substitute,
                disagreementSet, unify)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.Traversable (sequence)
import Partial.Unsafe (unsafePartial)

spec :: Spec Unit
spec = describe "Formulas" do
  substitutionTests
  disagreementSetTests
  unificationTests
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

    disagreementSetTests = describe "disagreement set" do
      it "handles empty input set"
        (disagreementSet Nil `shouldEqual` Nil)
      it "should work" do
        disagreementSet
          (List.fromFoldable
           [ List.fromFoldable [Var (Variable "x"), App "g" [Var (Variable "y"), Var (Variable "z")]]
           , List.fromFoldable [Var (Variable "x"), App "a" []]
           , List.fromFoldable [Var (Variable "x"), App "h" [App "k" [Var (Variable "x")]]] ])
          `shouldEqual` (List.fromFoldable
                         [ App "g" [Var (Variable "y"), Var (Variable "z")]
                         , App "a" []
                         , App "h" [App "k" [Var (Variable "x")]] ])
      it "detects differing arities"
        (disagreementSet (List.fromFoldable
                          [ Cons (App "f" [Var (Variable "x")]) Nil
                          , Cons (App "f" []) Nil ])
         `shouldEqual` (List.fromFoldable
                        [ App "f" [Var (Variable "x")]
                        , App "f" [] ]))

    unificationTests = describe "unification" do
      it "can find unifier" do
        unify (Set.fromFoldable
              [ List.fromFoldable [App "a" [], Var (Variable "x"), App "f" [App "g" [Var (Variable "y")]]]
              , List.fromFoldable [Var (Variable "z"), App "f" [Var (Variable "z")], App "f" [Var (Variable "u")]]
              ]) `shouldEqual` let
          σ = fold $ unsafePartial $ fromJust $ sequence
              [ singleSub (Variable "z") (App "a" [])
              , singleSub (Variable "x") (App "f" [App "a" []])
              , singleSub (Variable "u") (App "g" [Var $ Variable "y"])
              ]
          w' = List.fromFoldable
               [App "a" [], App "f" [App "a" []], App "f" [App "g" [Var $ Variable "y"]]]
          in Just $ Tuple σ w'
      it "does not try to substitute a variable with multiple terms" do
        unify (Set.fromFoldable
              [ List.fromFoldable [App "f" [App "a" []], App "g" [Var $ Variable "x"]]
              , List.fromFoldable [Var (Variable "y"), Var (Variable "y")]
              ])
          `shouldEqual` Nothing
