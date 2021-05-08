module Test.Proof where

import Prelude
import Formula (Formula(..))
import FormulaOrVar (FFC(FC))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List (List(Nil), (:))
import Data.Set as Set
import Proof (Rule(..), addProof, closeBox, openBox, runND)
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

{- | Some tests that are constructing proofs -}
testInference :: Spec Unit
testInference =
  describe "proof tests" do
    it "can do implies introduction"
      let
        (Tuple completed proof) =
          runND (Just $ FC (Implies p b))
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
          runND (Just $ FC (And a b))
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
          runND (Just $ FC a)
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
