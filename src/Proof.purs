module Proof where

import Data.Array (concat)
import Data.Either (Either)
import Data.Foldable (elem)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Formula (Formula, Term)
import Inference (Rule)

data ProofElement
  = BoxOpen
  | BoxClose
  | RowElem Row

type RowError
  = Either String

type Proof
  = Array ProofElement

type Row
  = { formula :: Maybe Formula
    , rule :: Maybe Rule
    , args :: Array Int
    }

type ValidationEnv
  = { provenFormulas :: Array (RowError Formula)
    , nextRow :: Int
    , lineScopes :: Array (Array Int)
    , boxScopes :: Array (Array (Tuple Int Int))
    , termScopes :: Array (Array Term)
    }

newEnv :: ValidationEnv
newEnv =
  { provenFormulas: []
  , nextRow: 1
  , lineScopes: [ [] ]
  , boxScopes: [ [] ]
  , termScopes: [ [] ]
  }

lineInScope :: Int -> ValidationEnv -> Boolean
lineInScope n ve = elem n (concat ve.lineScopes)

boxInScope :: Tuple Int Int -> ValidationEnv -> Boolean
boxInScope b ve = elem b (concat ve.boxScopes)

termInScope :: Term -> ValidationEnv -> Boolean
termInScope t ve = elem t (concat ve.termScopes)
