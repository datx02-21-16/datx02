module Test.GUI.Hint where

import Prelude
import GUI.Hint (genHint)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "hints" do
    it "can generate hint" do
      genHint { premises: [ "b" ], conclusion: "a → a ∧ b" } `shouldEqual` "Try starting with:\n\n Assume a"
