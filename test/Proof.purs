module Test.Proof where

import Prelude
import Formula (Formula(..))
import FormulaOrVar (FFC(FC))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List (List(Nil), (:))
import Data.Set as Set
import Proof (ND, Rule(..), addProof, closeBox, openBox, runND)
import Test.Formula (readFormula)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = testInference

a :: Formula
a = Predicate "A" []

b :: Formula
b = Predicate "B" []

p :: Formula
p = Predicate "P" []

boxed :: ND Unit -> ND Unit
boxed nd = openBox *> nd *> closeBox

{- | Some tests that are constructing proofs -}
testInference :: Spec Unit
testInference =
  describe "proof tests" do
    it "can do implies introduction"
      let
        (Tuple completed proof) =
          runND (Just $ Implies p b)
            $ do
                addProof
                  { formula: Just (FC $ Implies p (And a b))
                  , rule: Just Premise
                  }
                openBox
                addProof
                  { formula: Just (FC p)
                  , rule: Just Assumption
                  }
                addProof
                  { formula: Just (FC $ And a b)
                  , rule: Just (ImplElim (Just 1) (Just 2))
                  }
                addProof
                  { formula: Just (FC b)
                  , rule: Just (AndElim2 (Just 3))
                  }
                closeBox
                addProof
                  { formula: Just (FC $ Implies p b)
                  , rule: Just (ImplIntro (Just $ Tuple 2 4))
                  }
      in
        do
          completed `shouldEqual` true
          proof
            `shouldEqual`
              { rows:
                  [ { error: Nothing
                    , formula: (Just (FC $ Implies p (And a b)))
                    , rule: (Just Premise)
                    }
                  , { error: Nothing
                    , formula: (Just $ FC p)
                    , rule: (Just Assumption)
                    }
                  , { error: Nothing
                    , formula: (Just $ FC (And a b))
                    , rule: (Just (ImplElim (Just 1) (Just 2)))
                    }
                  , { error: Nothing
                    , formula: Just $ FC b
                    , rule: Just (AndElim2 (Just 3))
                    }
                  , { error: Nothing
                    , formula: Just $ FC (Implies p b)
                    , rule: Just (ImplIntro (Just $ Tuple 2 4))
                    }
                  ]
              , scopes:
                  ( { boxes: [ Tuple 2 4 ]
                    , lines: Set.insert 5 (Set.singleton 1) --[ 5, 1 ]
                    , boxStart: Nothing
                    }
                      : Nil
                  )
              }
    it "can do and intro"
      let
        (Tuple completed proof) =
          runND (Just $ And a b)
            $ do
                addProof
                  { formula: Just $ FC a
                  , rule: Just Premise
                  }
                addProof
                  { formula: Just $ FC b
                  , rule: Just Premise
                  }
                addProof
                  { formula: Just $ FC (And a b)
                  , rule: Just (AndIntro (Just 1) (Just 2))
                  }
      in
        do
          completed `shouldEqual` true
          proof
            `shouldEqual`
              { rows:
                  [ { error: Nothing
                    , formula: (Just $ FC a)
                    , rule: (Just Premise)
                    }
                  , { error: Nothing
                    , formula: (Just $ FC b)
                    , rule: (Just Premise)
                    }
                  , { error: Nothing
                    , formula: (Just $ FC (And a b))
                    , rule: (Just (AndIntro (Just 1) (Just 2)))
                    }
                  ]
              , scopes:
                  ( { boxes: []
                    , lines: Set.insert 3 (Set.insert 2 (Set.singleton 1)) --[ 3, 2, 1 ]
                    , boxStart: Nothing
                    }
                      : Nil
                  )
              }
    it "can do and elim 1"
      let
        (Tuple completed proof) =
          runND (Just $ a)
            $ do
                addProof
                  { formula: Just $ FC (And a b)
                  , rule: Just Premise
                  }
                addProof
                  { formula: Just $ FC a
                  , rule: Just (AndElim1 (Just 1))
                  }
      in
        do
          completed `shouldEqual` true
          proof
            `shouldEqual`
              { rows:
                  [ { error: Nothing
                    , formula: (Just $ FC (And a b))
                    , rule: (Just Premise)
                    }
                  , { error: Nothing
                    , formula: (Just $ FC a)
                    , rule: (Just (AndElim1 (Just 1)))
                    }
                  ]
              , scopes:
                  ( { boxes: []
                    , lines: Set.insert 2 (Set.singleton 1) --[ 2, 1 ]
                    , boxStart: Nothing
                    }
                      : Nil
                  )
              }
    it "can prove Ana's funky proof"
      let
        Tuple completed _ =
          runND (Just $ readFormula "¬P(a) → P(b)") do
            addProof { formula: Just $ FC $ readFormula "∀x (x = a ∨ x = b)", rule: Just Premise }
            addProof { formula: Just $ FC $ readFormula "∃xP(x)", rule: Just Premise }
            boxed do
              addProof { formula: Just $ FC $ readFormula "¬P(a)", rule: Just Assumption }
              boxed do
                addProof { formula: Just $ FC $ readFormula "P(x0)", rule: Just Assumption }
                addProof { formula: Just $ FC $ readFormula "x0 = a ∨ x0=b", rule: Just $ ForallElim (Just 1) }
                boxed do
                  addProof { formula: Just $ FC $ readFormula "x0=a", rule: Just Assumption }
                  addProof { formula: Just $ FC $ readFormula "P(a)", rule: Just $ EqElim (Just 6) (Just 4) }
                  addProof { formula: Just $ FC $ readFormula "⊥", rule: Just $ NegElim (Just 3) (Just 7) }
                  addProof { formula: Just $ FC $ readFormula "P(b)", rule: Just $ BottomElim (Just 8) }
                boxed do
                  addProof { formula: Just $ FC $ readFormula "x0=b", rule: Just Assumption }
                  addProof { formula: Just $ FC $ readFormula "P(b)", rule: Just $ EqElim (Just 10) (Just 4) }
                addProof { formula: Just $ FC $ readFormula "P(b)", rule: Just $ OrElim (Just 5) (Just $ Tuple 6 9) (Just $ Tuple 10 11) }
              addProof { formula: Just $ FC $ readFormula "P(b)", rule: Just $ ExistsElim (Just 2) (Just $ Tuple 4 12) }
            addProof { formula: Just $ FC $ readFormula "¬P(a)→P(b)", rule: Just $ ImplIntro (Just $ Tuple 3 13) }
      in
        completed `shouldEqual` true
