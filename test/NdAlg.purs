module Test.NdAlg where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (fromRight')
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import NdAlg (Rule(..), prove)
import Formula (Formula)
import Parser (parseFormula)

readFormula :: String -> Formula
readFormula s = fromRight' (\_ -> unsafeCrashWith "Bad formula")
                $ parseFormula s

spec :: Spec Unit
spec = describe "Automated ND" do
  it "should prove Peirce's law" do
    prove [] (readFormula "((P → Q) → P) → P")
      `shouldEqual` Just [ { formula: readFormula "(P → Q) → P", rule: Assumption }
                         , { formula: readFormula "¬P", rule: Assumption }
                         , { formula: readFormula "P", rule: Assumption }
                         , { formula: readFormula "¬Q", rule: Assumption }
                         , { formula: readFormula "¬¬Q", rule: NotIntro 3 }
                         , { formula: readFormula "Q", rule: NotElim 4 }
                         , { formula: readFormula "P → Q", rule: ImpliesIntro 5 }
                         , { formula: readFormula "P", rule: ImpliesElim 0 6 }
                         , { formula: readFormula "¬¬P", rule: NotIntro 7 }
                         , { formula: readFormula "P", rule: NotElim 8 }
                         , { formula: readFormula "((P → Q) → P) → P", rule: ImpliesIntro 9 } ]

  it "should prove modus tollens" do
    prove [readFormula "P → Q", readFormula "¬Q"]
      (readFormula "¬P")
      `shouldEqual` Just [ { formula: readFormula "P → Q", rule: Premise }
                         , { formula: readFormula "¬Q", rule: Premise }
                         , { formula: readFormula "P", rule: Assumption }
                         , { formula: readFormula "Q", rule: ImpliesElim 0 2 }
                         , { formula: readFormula "¬P", rule: NotIntro 3 } ]

  it "should prove law of excluded middle" do
    prove [] (readFormula "A ∨ ¬A")
      `shouldEqual` Just [ { formula: readFormula "¬(A ∨ ¬A)", rule: Assumption }
                         , { formula: readFormula "A", rule: Assumption }
                         , { formula: readFormula "(A ∨ ¬A)", rule: OrIntro }
                         , { formula: readFormula "¬A", rule: NotIntro 2 }
                         , { formula: readFormula "¬A", rule: Assumption }
                         , { formula: readFormula "(A ∨ ¬A)", rule: OrIntro }
                         , { formula: readFormula "¬¬A", rule: NotIntro 5 }
                         , { formula: readFormula "¬¬(A ∨ ¬A)", rule: NotIntro 6 }
                         , { formula: readFormula "(A ∨ ¬A)", rule: NotElim 7 } ]

  it "should determine that no ND proof can be found" do
    prove [readFormula "P"] (readFormula "Q") `shouldEqual` Nothing

  -- it "should be able to do hard shit" do
    -- prove [readFormula "(P∧Q)→(R∨S)"] (readFormula "(P→R)∨(Q→S)") `shouldEqual` Nothing

    -- Commented out because it takes ~5 seconds
    -- prove [readFormula "(P∧Q)→(R∨S)"] (readFormula "(P→R)∨(Q→V)") `shouldEqual` Nothing

  -- This one does not work (never terminates)
  -- it "should be able to do very hard shit" do
    -- prove [] (readFormula "(P∧Q)∨(P∧¬Q)∨(¬P∧Q)∨(¬P∧¬Q)") `shouldEqual` Nothing

  it "should prove De Morgan's" do
    prove [readFormula "¬(A ∧ B)"] (readFormula "¬A ∨ ¬B")
      `shouldEqual` Just [ { formula: readFormula "¬(A ∧ B)", rule: Premise }
                         , { formula: readFormula "¬(¬A ∨ ¬B)", rule: Assumption }
                         , { formula: readFormula "¬A", rule: Assumption }
                         , { formula: readFormula "¬A ∨ ¬B", rule: OrIntro }
                         , { formula: readFormula "¬¬A", rule: NotIntro 3 }
                         , { formula: readFormula "A", rule: NotElim 4 }
                         , { formula: readFormula "¬B", rule: Assumption }
                         , { formula: readFormula "¬A ∨ ¬B", rule: OrIntro }
                         , { formula: readFormula "¬¬B", rule: NotIntro 7 }
                         , { formula: readFormula "B", rule: NotElim 8 }
                         , { formula: readFormula "A ∧ B", rule: AndIntro }
                         , { formula: readFormula "¬¬(¬A ∨ ¬B)", rule: NotIntro 10 }
                         , { formula: readFormula "¬A ∨ ¬B", rule: NotElim 11 } ]
    prove [readFormula "¬A ∨ ¬B"] (readFormula "¬(A ∧ B)")
      `shouldEqual` Just [ { formula: readFormula "¬A ∨ ¬B", rule: Premise }
                         , { formula: readFormula "A ∧ B", rule: Assumption }
                         , { formula: readFormula "A", rule: AndElim 1 }
                         , { formula: readFormula "B", rule: AndElim 1 }
                         , { formula: readFormula "¬A", rule: Assumption }
                         , { formula: readFormula "¬¬A", rule: NotIntro 4 }
                         , { formula: readFormula "¬B", rule: OrElim 0 5 }
                         , { formula: readFormula "¬(A ∧ B)", rule: NotIntro 6 } ]
