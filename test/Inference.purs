module Test.Inference where

import Prelude

import Data.Either (Either(..))
import Formula (Formula(..))
import Inference (NDErrors(..), andElimL, andElimR, andIntro, doubleNotElim, doubleNotIntro, implIntro, modusTollens, notElim, orElim, runND)
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

  it "Cant execute a conjunction elimination (^e2) when applied to a wrong formula (this test case : implies formula) " do
    (runND (andElimR (Implies a b))) `shouldEqual`
        Left (NotAConjunction (Implies a b))

  it "Can execute a conjunction elimination (^e2) when applied to a correct formula (conjunction formula)" do
    (runND (andElimR (And a b))) `shouldEqual`
        (Right b)

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
  it "Test: Cant execute double negation elimination (¬¬e) on wrong formula (not a ¬¬Φ)" do
    (runND (doubleNotElim (And a b))) `shouldEqual`
      (Left (NotADoubleNeg (And a b)))
  it "Test: Can execute double negation elimination (¬¬e) on correct formula (a ¬¬Φ)" do
    (runND (doubleNotElim (Not(Not(And a b))))) `shouldEqual`
      (Right (And a b))
  it "Test: Cant execute modus tollens (MT) on wrong formulas (Wrong Input: Φ → Ψ1 , ¬Ψ2 where Ψ1 != Ψ2 )" do
    (runND (modusTollens (Implies a b)(Not a))) `shouldEqual`
      (Left (BadModusTollens (a)(b)(a)))
  it "Test: Can execute modus tollens (MT) on correct formulas (Correct Input: Φ → Ψ1 , ¬Ψ2 where Ψ1 == Ψ2 )" do
    (runND (modusTollens (Implies a b)(Not b))) `shouldEqual`
      (Right (Not a))
  it "Test: Cant execute modus tollens (MT) on wrong formulas (Wrong Input: Φ ∧ Ψ , Φ ∨ Ψ )" do
    (runND (modusTollens (And a b)(Or a b))) `shouldEqual`
      (Left (NotAModusTollens (And a b) (Or a b)))
  it "Test: Can execute double negation introduction (¬¬i) on a formula (Input: ¬Φ)" do
    (runND (doubleNotIntro (Not a)) `shouldEqual`
      (Right (Not (Not (Not a)))))
  it "Test: Can execute negation elimination (¬e) on correct formulas (Input: Φ , ¬Φ)" do
    (runND (notElim (And a b)(Not (And a b))) `shouldEqual`
      (Right Bottom))
  it "Test: Cant execute negation elimination (¬e) on wrong formulas (Wrong Input: Φ , Φ)" do
    (runND (notElim (And a b)(And a b)) `shouldEqual`
      (Left (NotANegElim (And a b)(And a b))))
  it "Test: Cant execute negation elimination (¬e) on wrong formulas (Wrong Input: Φ1 , ¬Φ2 where Φ1 != Φ2)" do
    (runND (notElim (And a b)(Not(Or a b))) `shouldEqual`
      (Left (BadNegElim (And a b)(Not(Or a b)))))
  