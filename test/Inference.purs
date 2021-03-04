module Test.Inference where

import Prelude

import Data.Array (singleton)
import Formula (Formula(..), Variable(..), Term(..))
import Data.Either (Either(..))
import Inference (NDErrors(..), andElimL, andElimR, andIntro, closeBox, doubleNotElim, forallIntro, doubleNotIntro, implElim, implIntro, modusTollens, orElim, runND)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do testInference
          bookExamples

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

  it "cannot execute a conjunction elimination (^e2) when applied to a wrong formula (this test case : implies formula) " do
    (runND (andElimR (Implies a b))) `shouldEqual`
        Left (NotAConjunction (Implies a b))

  it "can execute a conjunction elimination (^e2) when applied to a correct formula (conjunction formula)" do
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
                                 andIntro b' a'))) `shouldEqual`
        (Right (Implies (And a b) (And b a)))
  it "Test: Cannot execute double negation elimination (¬¬e) on wrong formula (not a ¬¬Φ)" do
    (runND (doubleNotElim (And a b))) `shouldEqual`
      (Left (NotADoubleNeg (And a b)))
  it "Test: Can execute double negation elimination (¬¬e) on correct formula (a ¬¬Φ)" do
    (runND (doubleNotElim (Not(Not(And a b))))) `shouldEqual`
      (Right (And a b))
  it "Test: Cannot execute modus tollens (MT) on wrong formulas (Wrong Input: Φ → Ψ1 , ¬Ψ2 where Ψ1 != Ψ2 )" do
    (runND (modusTollens (Implies a b)(Not a))) `shouldEqual`
      (Left (BadModusTollens (a)(b)(a)))
  it "Test: Can execute modus tollens (MT) on correct formulas (Correct Input: Φ → Ψ1 , ¬Ψ2 where Ψ1 == Ψ2 )" do
    (runND (modusTollens (Implies a b)(Not b))) `shouldEqual`
      (Right (Not a))
  it "Test: Cannot execute modus tollens (MT) on wrong formulas (Wrong Input: Φ ∧ Ψ , Φ ∨ Ψ )" do
    (runND (modusTollens (And a b)(Or a b))) `shouldEqual`
      (Left (NotAModusTollens (And a b) (Or a b)))
  it "Test: Can execute double negation introduction (¬¬i) on a formula (Input: ¬Φ)" do
    (runND (doubleNotIntro (Not a)) `shouldEqual`
      (Right (Not (Not (Not a)))))
  it "properly substitutes in forall elim" do
    (runND (forallIntro
              (Variable "x0")
              (Variable "x")
              (pure (Predicate "P" (singleton (Var (Variable "x0"))))))) `shouldEqual`
      Right (Forall ((Variable "x")) (Predicate "P" (singleton (Var (Variable "x")))))
  --it "Test: Can execute negation elimination (¬e) on correct formulas (Input: Φ , ¬Φ)" do
  --  (runND (notElim (And a b)(Not (And a b))) `shouldEqual`
  --    (Right Bottom))
  --it "Test: Cant execute negation elimination (¬e) on wrong formulas (Wrong Input: Φ , Φ)" do
  --  (runND (notElim (And a b)(And a b)) `shouldEqual`
  --    (Left (NotANegElim (And a b)(And a b))))
  --it "Test: Cant execute negation elimination (¬e) on wrong formulas (Wrong Input: Φ1 , ¬Φ2 where Φ1 != Φ2)" do
  --  (runND (notElim (And a b)(Not(Or a b))) `shouldEqual`
  --    (Left (BadNegElim (And a b)(Not(Or a b)))))
  
bookExamples :: Spec Unit
bookExamples = describe "examples from the book Logic in Computer Science" do
  let p = Predicate "p" []
  let q = Predicate "q" []
  let r = Predicate "r" []

  it "page 13 example 1.9" do
    (runND (do let premise = Implies (Not q) (Not p)
               
               implIntro p (\p -> do
                   nnp <- doubleNotIntro p
                   nnq <- modusTollens premise nnp
                   closeBox nnq)

               )) `shouldEqual`
            Right (Implies p (Not (Not q)))

  it "page 13 example 1.11" do
    (runND (do
               implIntro (Implies q r) (\impqr -> do
                   long <- implIntro (Implies (Not q) (Not p)) (\impnqnp -> do
                               pimpr <- implIntro p (\p -> do
                                            nnp <- doubleNotIntro p
                                            nnq <- modusTollens impnqnp nnp
                                            q   <- doubleNotElim nnq
                                            r   <- implElim impqr q
                                            closeBox r)
                               closeBox pimpr)
                   closeBox long))) `shouldEqual`
            Right (Implies
                    (Implies q r)
                    (Implies                      
                      (Implies (Not q) (Not p))
                      (Implies p r)))

  it "page 15 example 1.13" do
    (runND (do let premise = Implies (And p q) (r)

               implIntro p (\p -> do
                 qr <- implIntro q (\q -> do
                   pq <- andIntro p q
                   r  <- implElim premise pq
                   closeBox r)
                 closeBox qr) 
               )) `shouldEqual`
            Right (Implies p (Implies q r))
  
  it "page 16 example 1.14" do
    (runND (do let premise = Implies (p) (Implies q r)
  
               implIntro (And p q) (\pq -> do
                 p  <- andElimL pq
                 q  <- andElimR pq 
                 qr <- implElim premise p
                 r  <- implElim qr q
                 closeBox r)
                )) `shouldEqual`
            Right (Implies (And p q) r)
  
  it "page 16 example 1.15" do
    (runND (do let premise = Implies p q
               implIntro (And p r) (\pr -> do
                 p  <- andElimL pr
                 r  <- andElimR pr
                 q  <- implElim premise p
                 qr <- andIntro q r
                 closeBox qr)
               )) `shouldEqual`
            Right (Implies (And p r) (And q r))

  it "page 47 example 1" do
    (runND (do let premise = Implies (And p q) r
               
               implIntro p (\p ->
                   implIntro q (\q -> do
                       pq <- andIntro p q
                       r  <- implElim premise pq
                       closeBox r))))
                       
        `shouldEqual`
        Right (Implies p (Implies q r))

  it "page 47 example 2" do
    (runND (do let premise1 = Implies (And p q) r
               let premise2 = p
               implIntro q (\q -> do
                   pq <- andIntro premise2 q
                   r  <- implElim premise1 pq
                   closeBox r)))
                       
        `shouldEqual`
        Right (Implies q r)
