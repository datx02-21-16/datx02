module GUI.Proof where

import Data.Maybe (Maybe(..))
import Formula as F

data Rule
  = Rule

data Proof
  = Proof
    { lines :: Array ProofLine
    , target :: Maybe F.Formula
    }

data ProofLine
  = ProofLine
    { number :: Int
    , text :: String
    , formula :: Maybe F.Formula
    , rule :: Maybe Rule
    }

emptyProof :: Proof
emptyProof = Proof { lines: [], target: Nothing }

newLine :: Int -> ProofLine
newLine n = ProofLine { number: n, text: "", formula: Nothing, rule: Nothing }
