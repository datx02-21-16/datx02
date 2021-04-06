module Test.Proof where

import Prelude (Unit, discard, ($))
import Formula (Formula(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Proof
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
          runND (Just (Implies p b))
            $ do
                addProof
                  { formula: Just (Implies p (And a b))
                  , rule: Just Premise
                  }
                openBox
                addProof
                  { formula: Just p
                  , rule: Just Assumption
                  }
                addProof
                  { formula: Just (And a b)
                  , rule: Just (ImplElim 1 2)
                  }
                addProof
                  { formula: Just b
                  , rule: Just (AndElim2 3)
                  }
                closeBox
                addProof
                  { formula: Just (Implies p b)
                  , rule: Just (ImplIntro (Tuple 2 4))
                  }
      in
        do
          completed `shouldEqual` true
          proof `shouldEqual` 
              { boxStarts: []
              , rows:
                  [ { error: Nothing
                    , formula: (Just (Implies p (And a b)))
                    , rule: (Just Premise)
                    }
                  , { error: Nothing
                    , formula: (Just p)
                    , rule: (Just Assumption)
                    }
                  , { error: Nothing
                    , formula: (Just (And a b))
                    , rule: (Just (ImplElim 1 2))
                    }
                  , { error: Nothing
                    , formula: Just b
                    , rule: Just (AndElim2 3)
                    }
                  , { error: Nothing
                    , formula: Just (Implies p b)
                    , rule: Just (ImplIntro (Tuple 2 4))}
                  ]
              , scopes:
                  [ { boxes: [Tuple 2 4]
                    , lines: [ 5, 1 ]
                    , vars: []
                    }
                  ]
              }              
    it "can do and intro"
      let
        (Tuple completed proof) =
          runND (Just (And a b))
            $ do
                addProof
                  { formula: Just a
                  , rule: Just Premise
                  }
                addProof
                  { formula: Just b
                  , rule: Just Premise
                  }
                addProof
                  { formula: Just (And a b)
                  , rule: Just (AndIntro 1 2)
                  }
      in
        do
          completed `shouldEqual` true
          proof
            `shouldEqual`
              { boxStarts: []
              , rows:
                  [ { error: Nothing
                    , formula: (Just a)
                    , rule: (Just Premise)
                    }
                  , { error: Nothing
                    , formula: (Just b)
                    , rule: (Just Premise)
                    }
                  , { error: Nothing
                    , formula: (Just (And a b))
                    , rule: (Just (AndIntro 1 2))
                    }
                  ]
              , scopes:
                  [ { boxes: []
                    , lines: [ 3, 2, 1 ]
                    , vars: []
                    }
                  ]
              }
    it "can do and elim 1"
      let
        (Tuple completed proof) =
          runND (Just a)
            $ do
                addProof
                  { formula: Just (And a b)
                  , rule: Just Premise
                  }
                addProof
                  { formula: Just a
                  , rule: Just (AndElim1 1)
                  }
      in
        do
          completed `shouldEqual` true
          proof
            `shouldEqual`
              { boxStarts: []
              , rows:
                  [ { error: Nothing
                    , formula: (Just (And a b))
                    , rule: (Just Premise)
                    }
                  , { error: Nothing
                    , formula: (Just a)
                    , rule: (Just (AndElim1 1))
                    }
                  ]
              , scopes:
                  [ { boxes: []
                    , lines: [ 2, 1 ]
                    , vars: []
                    }
                  ]
              }
