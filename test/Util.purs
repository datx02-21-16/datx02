module Test.Util where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.Array ((..))

import Util (moveWithin)

spec :: Spec Unit
spec = describe "Util" do
  describe "moveWithin" do
    it "can move a part to the left"
      $ moveWithin 1 2 4 (1 .. 5) `shouldEqual` [1, 3, 4, 2, 5]
    it "can move a part to the right"
      $ moveWithin 3 0 1 (1 .. 5) `shouldEqual` [2, 3, 1, 4, 5]
    it "should handle moving a part to itself" let
      xs = 1 .. 5
      in moveWithin 2 2 3 xs `shouldEqual` xs
