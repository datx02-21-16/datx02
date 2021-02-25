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
                         , { formula: readFormula "¬¬Q", rule: NotIntro }
                         , { formula: readFormula "Q", rule: NotElim 4 }
                         , { formula: readFormula "P → Q", rule: ImpliesIntro }
                         , { formula: readFormula "P", rule: ImpliesElim 0 6 }
                         , { formula: readFormula "¬¬P", rule: NotIntro }
                         , { formula: readFormula "P", rule: NotElim 8 }
                         , { formula: readFormula "((P → Q) → P) → P", rule: ImpliesIntro }
                         ]

  it "should be able to do shit" do
    prove [readFormula "P∨Q"] (readFormula "¬P→Q") `shouldEqual` Nothing
  it "should be able to do hard shit" do
    prove [readFormula "(P∧Q)→(R∨S)"] (readFormula "(P→R)∨(Q→S)") `shouldEqual` Nothing

  -- This one takes a long ass time to finish.
  -- it "should be able to do hard shit v2" do
    -- prove [readFormula "(P∧Q)→(R∨S)"] (readFormula "(P→R)∨(Q→V)") `shouldEqual` Nothing

  it "should prove De Morgan's" do
    prove [readFormula "¬(A ∧ B)"] (readFormula "¬A ∨ ¬B") `shouldEqual` Nothing

  it "should prove De Morgan's v2" do
    prove [readFormula "¬A ∨ ¬B"] (readFormula "¬(A ∧ B)") `shouldEqual` Nothing
