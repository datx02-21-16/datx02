module Test.Formula where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.QuickCheck (class Arbitrary, arbitrary, assertEquals)
import Test.QuickCheck.Gen (Gen, oneOf, vectorOf, sized, chooseInt, resize)
import Data.List as List
import Data.List (List(Nil, Cons))
import Data.NonEmpty (NonEmpty(NonEmpty))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Set as Set
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromJust)
import Data.Either (Either(Right), fromRight')
import Data.Tuple (Tuple(..))
import Data.Traversable (sequence)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Control.Apply (lift2)
import Data.Newtype (class Newtype, unwrap, un)
import Control.Lazy (fix)
import Data.Char as Char
import Data.String.CodeUnits as CU
import Formula
  ( Term(..)
  , Variable(..)
  , Formula(..)
  , singleSub
  , varSub
  , substitute
  , disagreementSet
  , unify
  , formulaUnifier
  )
import Parser (parseFormula)

-- | Generator for a single uppercase letter string.
upper :: Gen String
upper = do
  i <- chooseInt 65 90
  pure $ CU.singleton $ unsafePartial $ fromJust $ Char.fromCharCode i

-- | Generator for a single lowercase letter string.
lower :: Gen String
lower = do
  i <- chooseInt 97 122
  pure $ CU.singleton $ unsafePartial $ fromJust $ Char.fromCharCode i

-- | A generator for a variable symbol.
variable :: Gen Variable
variable = Variable <$> lower

-- Due to orphan rules we need to wrap terms/formulas in newtypes in
-- order to add arbitrary instances. Also have to make sure to limit
-- the depth when generating these recursive data types.
newtype TTerm
  = TTerm Term

derive instance newtypeTTerm :: Newtype TTerm _

instance arbitraryTTerm :: Arbitrary TTerm where
  arbitrary = sized $ fix f
    where
    f p n
      | n > 0 = TTerm <$> (oneOf $ NonEmptyArray.fromNonEmpty $ NonEmpty (Var <$> variable) [ term ])
        where
        term = do
          m <- chooseInt 0 (n / 2)
          lift2 App lower $ vectorOf m (unwrap <$> p (n / (m + 1)))

    f p _ = TTerm <<< Var <$> variable

newtype TFormula
  = TFormula Formula

derive instance newtypeTFormula :: Newtype TFormula _

instance arbitraryTFormula :: Arbitrary TFormula where
  arbitrary = sized $ fix f 0
    where
    f p minArgs n =
      TFormula
        <$> let
            args = do
              m <- chooseInt (max minArgs 0) (max minArgs (n / 2))
              vectorOf m $ resize (n / (m + 1)) (un TTerm <$> arbitrary)

            atom = lift2 Predicate upper args
          in
            if n <= 0 then
              atom
            else
              let
                arbFormula = unwrap <$> p minArgs (n / 2)

                arbFOLFormula = unwrap <$> p 1 (n / 2)
              in
                oneOf $ NonEmptyArray.fromNonEmpty
                  $ NonEmpty atom
                      [ Not <$> arbFormula
                      , lift2 And arbFormula arbFormula
                      , lift2 Or arbFormula arbFormula
                      , lift2 Implies arbFormula arbFormula
                      , lift2 Forall variable arbFOLFormula
                      , lift2 Exists variable arbFOLFormula
                      ]

readFormula :: String -> Formula
readFormula s = fromRight' (\_ -> unsafeCrashWith "Bad formula") $ parseFormula s

spec :: Spec Unit
spec =
  describe "Formulas" do
    substitutionTests
    disagreementSetTests
    unificationTests
    showTests
  where
  substitutionTests =
    describe "substitutions" do
      it "can do substitution on formula" do
        let
          θ =
            fold $ unsafePartial $ fromJust
              $ sequence
                  [ singleSub (Variable "x") (App "a" [])
                  , singleSub (Variable "y") (App "f" [ App "b" [] ])
                  , singleSub (Variable "z") (App "c" [])
                  ]

          e = Predicate "P" [ Var (Variable "x"), Var (Variable "y"), Var (Variable "z") ]
        substitute θ e
          `shouldEqual`
            Predicate "P" [ App "a" [], App "f" [ App "b" [] ], App "c" [] ]

  disagreementSetTests =
    describe "disagreement set" do
      it "handles empty input set"
        (disagreementSet Nil `shouldEqual` Nil)
      it "should work" do
        disagreementSet
          ( List.fromFoldable
              [ List.fromFoldable [ Var (Variable "x"), App "g" [ Var (Variable "y"), Var (Variable "z") ] ]
              , List.fromFoldable [ Var (Variable "x"), App "a" [] ]
              , List.fromFoldable [ Var (Variable "x"), App "h" [ App "k" [ Var (Variable "x") ] ] ]
              ]
          )
          `shouldEqual`
            ( List.fromFoldable
                [ App "g" [ Var (Variable "y"), Var (Variable "z") ]
                , App "a" []
                , App "h" [ App "k" [ Var (Variable "x") ] ]
                ]
            )
      it "detects differing arities"
        ( disagreementSet
            ( List.fromFoldable
                [ Cons (App "f" [ Var (Variable "x") ]) Nil
                , Cons (App "f" []) Nil
                ]
            )
            `shouldEqual`
              ( List.fromFoldable
                  [ App "f" [ Var (Variable "x") ]
                  , App "f" []
                  ]
              )
        )

  unificationTests =
    describe "unification" do
      it "does not unify empty set" $ unify Set.empty `shouldEqual` Nothing
      it "can find unifier" do
        unify
          ( Set.fromFoldable
              [ List.fromFoldable [ App "a" [], Var (Variable "x"), App "f" [ App "g" [ Var (Variable "y") ] ] ]
              , List.fromFoldable [ Var (Variable "z"), App "f" [ Var (Variable "z") ], App "f" [ Var (Variable "u") ] ]
              ]
          )
          `shouldEqual`
            let
              σ =
                fold $ unsafePartial $ fromJust
                  $ sequence
                      [ singleSub (Variable "z") (App "a" [])
                      , singleSub (Variable "x") (App "f" [ App "a" [] ])
                      , singleSub (Variable "u") (App "g" [ Var $ Variable "y" ])
                      ]

              w' =
                List.fromFoldable
                  [ App "a" [], App "f" [ App "a" [] ], App "f" [ App "g" [ Var $ Variable "y" ] ] ]
            in
              Just $ Tuple σ w'
      it "does not try to substitute a variable with multiple terms" do
        unify
          ( Set.fromFoldable
              [ List.fromFoldable [ App "f" [ App "a" [] ], App "g" [ Var $ Variable "x" ] ]
              , List.fromFoldable [ Var (Variable "y"), Var (Variable "y") ]
              ]
          )
          `shouldEqual`
            Nothing
      it "can unify propositional logic formula" do
        let
          f = And (Predicate "A" []) (Predicate "B" [])
        formulaUnifier f f `shouldEqual` Just mempty
      it "can unify simple predicate"
        $ formulaUnifier (Predicate "P" [ Var x ]) (Predicate "P" [ Var y ])
            `shouldEqual`
              Just (varSub x y)
      it "can unify quantified formulas" do
        formulaUnifier
          (Forall x $ Predicate "P" [ Var x ])
          (Forall y $ Predicate "P" [ Var y ])
          `shouldEqual`
            Just mempty
        formulaUnifier
          (Forall x $ Predicate "P" [ Var x, Var y ])
          (Forall x $ Predicate "P" [ Var x, Var z ])
          `shouldEqual`
            Just (varSub y z)
      it "does not unify bound and unbound variables" do
        let
          a = Forall x $ Predicate "P" [ Var x ]

          b = Forall y $ Predicate "P" [ Var x ]
        formulaUnifier a b `shouldEqual` Nothing
        formulaUnifier b a `shouldEqual` Nothing
      it "unifies complex formula"
        let
          a = Forall y $ Implies (Predicate "P" [ Var x ]) (Predicate "Q" [ Var y ])

          b = Forall y $ Implies (Predicate "P" [ Var $ Variable "x0" ]) (Predicate "Q" [ Var y ])
        in
          formulaUnifier a b `shouldEqual` Just (varSub x (Variable "x0"))

  showTests =
    describe "show" do
      it "should handle precedence" do
        show (Or (Predicate "A" []) (Not $ Predicate "A" []))
          `shouldEqual`
            "A ∨ ¬A"
        show (Exists (Variable "x") (Implies (Predicate "P" [ Var $ Variable "x" ]) (Predicate "Q" [ Var $ Variable "x" ])))
          `shouldEqual`
            "∃x (P(x) → Q(x))"
      it "should handle associativity" do
        show (And (And (Predicate "A" []) (Predicate "B" [])) (Predicate "C" []))
          `shouldEqual`
            "A ∧ B ∧ C"
        show (And (Predicate "A" []) (And (Predicate "B" []) (Predicate "C" [])))
          `shouldEqual`
            "A ∧ (B ∧ C)"
      it "should format equality nicely" do
        show (Predicate "=" [ Var x, Var y ]) `shouldEqual` "x = y"
      it "should survive show/parseFormula roundtrip"
        $ quickCheck \(TFormula formula) ->
            parseFormula (show formula) `assertEquals` Right formula

  x = Variable "x"

  y = Variable "y"

  z = Variable "z"
