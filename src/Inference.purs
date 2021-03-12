module Inference where

import Prelude

data Rule
  = AndElimE1
  | AndElimE2
  | AndIntro
  | ImplElim
  | ImplIntro
  | Premise
  | Assume
  | BottomElim
  | DoubleNegElim
  | NegElim
  | ModusTollens
  | DoubleNegIntro

--data RuleApp = {rule :: Rule , formulas :: Array Formula}
instance showRule :: Show Rule where
  show (AndElimE1) = "^E1"
  show (AndElimE2) = "^E2"
  show (AndIntro) = "^I"
  show (ImplElim) = "->e"
  show (ImplIntro) = "->i"
  show (Assume) = "Assume"
  show (Premise) = "Premise"
  show (BottomElim) = "Bottom elimination"
  show (DoubleNegElim) = "Double neg elimination"
  show (NegElim) = "Neg elimination"
  show (ModusTollens) = "MT"
  show (DoubleNegIntro) = "Double neg introduction"
