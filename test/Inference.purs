module Test.Inference where

import Prelude
import Data.Either (Either(..))

import Formula (Formula(..))
import Inference (NDErrors(..), runND, andElimL, andElimR, andIntro, orElim, implIntro)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = testInference

-- | Shorthand for the formula "A"
a :: Formula
a = Predicate "A" []

-- | Shorthand for the formula "B"
b :: Formula
b = Predicate "B" []

-- | Shorthand for the formula "C"
c :: Formula
c = Predicate "C" []

-- | Shorthand for the formula "R"
r :: Formula
r = Predicate "R" []

{- | Some tests attempting to evaluate the functionality of the attempted
inference machinery. -}
testInference :: Spec Unit
testInference = describe "inference tests" do
  it "cannot do and elimination when applied to or" do
    (runND (andElimL (Or a b))) `shouldEqual`
        Left (NotAConjunction (Or a b))

  it "can do and elimination to and" do
    (runND (andElimL (And a b))) `shouldEqual`
        (Right a)

  it "can commute conjunction" do
    (runND (do let formula = And a b
               a' <- andElimL formula
               b' <- andElimR formula
               andIntro b' a')) `shouldEqual`
        (Right (And b a))

  it "can eliminate or" do
    (runND (do let formula = Or (And r a) (And b r)
               orElim formula andElimL andElimR)) `shouldEqual`
        (Right r)

  it "can introduce implications" do
    (runND (implIntro (And a b) 
                      (\ab -> do a'  <- andElimL ab
                                 b'  <- andElimR ab
                                 ba <- andIntro b' a'
                                 pure $ Implies ab ba))) `shouldEqual`
        (Right (Implies (And a b) (And b a)))