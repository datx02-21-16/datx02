module GUI.Rules (RuleType(..), rules) where

import Data.Show

data RuleType
  = RtPremise
  | RtAssumption
  | AndElim1
  | AndElim2
  | AndIntro
  | OrElim
  | OrIntro1
  | OrIntro2
  | ImplElim
  | ImplIntro
  | NegElim
  | NegIntro
  | BottomElim
  | DoubleNegElim
  | ModusTollens
  | DoubleNegIntro
  | PBC
  | LEM
-- We need to add copy here

instance showRuleType :: Show RuleType where
  show r = case r of
    RtPremise -> "Premise"
    RtAssumption -> "Ass."
    AndElim1 -> "∧e1"
    AndElim2 -> "∧e2"
    AndIntro -> "∧i"
    OrElim -> "∨e"
    OrIntro1 -> "∨i1"
    OrIntro2 -> "∨i2"
    ImplElim -> "→e"
    ImplIntro -> "→i"
    NegElim -> "¬e"
    NegIntro -> "¬i"
    BottomElim -> "⊥e"
    DoubleNegElim -> "¬¬e"
    ModusTollens -> "MT"
    DoubleNegIntro -> "¬¬i"
    PBC -> "PBC"
    LEM -> "LEM"

rules :: Array RuleType
rules =
  [ RtPremise
  , RtAssumption
  , AndElim1
  , AndElim2
  , AndIntro
  , OrElim
  , OrIntro1
  , OrIntro2
  , ImplElim
  , ImplIntro
  , NegElim
  , NegIntro
  , BottomElim
  , DoubleNegElim
  , ModusTollens
  , DoubleNegIntro
  , PBC
  , LEM
  ]