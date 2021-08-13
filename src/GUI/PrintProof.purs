module GUI.PrintProof where

import Prelude
import Data.Array
import Data.Maybe

--printProof :: {row :: Array String} -> String
--printProof :: String
--printProof :: forall t1. t1 -> t1
--printProof xs = xs

printProof arr = case uncons arr of
  Just { head: x, tail: xs } -> x
  Nothing -> {}







