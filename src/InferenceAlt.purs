module InferenceAlt where 

import Data.Either
import Data.Maybe
import Data.String
import Formula
import Prelude
import Data.Array (deleteAt, head, index, insert, insertAt, length, tail , range)
import Partial.Unsafe as Partial


data Rule = AndElimE1 | AndElimE2 | AndIntro | ImplElim | ImplIntro | Premise | Assume | BottomElim | DoubleNegElim

instance showRule :: Show Rule where 
  show (AndElimE1)  = "AND E1"
  show (AndElimE2)  = "AND E2" 
  show (AndIntro)   = "And Intro"
  show (ImplElim)   = "Implies Elimination"  
  show (ImplIntro)  = "Implies Introduction"
  show (Assume)     = "Assume"
  show (Premise)    = "Premise"
  show (BottomElim) = "Bottom elimination"
  show (DoubleNegElim) = "Double neg elimination"

