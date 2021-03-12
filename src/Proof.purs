module Proof where

import Control.Bind
import Control.Monad.State (State, modify_, gets)
import Data.Array (concat, snoc, init, last, length, modifyAt)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Function (($))
import Data.Maybe (Maybe, fromJust)
import Data.Tuple (Tuple(..))
import Formula (Formula, Term)
import Inference (Rule)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, identity, (+), (-))

type Validation
  = State ValidationEnv

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
    , boxFrom :: Array Int
    , lineScopes :: Array (Array Int)
    , boxScopes :: Array (Array (Tuple Int Int))
    , termScopes :: Array (Array Term)
    }

-- | Validate a line.
validateLine :: ProofElement -> Validation Unit
validateLine = case _ of
  BoxOpen -> enterBox
  BoxClose -> exitBox
  RowElem r -> modify_ identity -- TODO

-- | Create an empty environment
newEnv :: ValidationEnv
newEnv =
  { provenFormulas: []
  , nextRow: 1
  , boxFrom: []
  , lineScopes: [ [] ]
  , boxScopes: [ [] ]
  , termScopes: [ [] ]
  }

-- State updates
addFormula :: Formula -> Validation Unit
addFormula f = modify_ \s -> s { provenFormulas = snoc s.provenFormulas (Right f) }

-- | Increment nextRow.
incNextRow :: Validation Unit
incNextRow = modify_ \s -> s { nextRow = s.nextRow + 1 }

-- | Enter a new box.
enterBox :: Validation Unit
enterBox =
  modify_
    ( \s ->
        s
          { lineScopes = snoc s.lineScopes []
          , boxScopes = snoc s.boxScopes []
          , termScopes = snoc s.termScopes []
          , boxFrom = snoc s.boxFrom s.nextRow
          }
    )

-- | Exit from a box.
exitBox :: Validation Unit
exitBox = do
  dropScope
  bStart <- gets $ \s -> unsafePartial $ fromJust $ last $ s.boxFrom
  bEnd <- gets _.nextRow
  addBoxToScope $ Tuple bStart bEnd
  modify_ \s -> s { boxFrom = unsafePartial $ fromJust $ init s.boxFrom }

-- | Add a box to the innermost box scope in the environment
addBoxToScope :: Tuple Int Int -> Validation Unit
addBoxToScope b =
  modify_
    ( \state ->
        state
          { boxScopes =
            unsafePartial
              $ fromJust
              $ modifyAt
                  (lastIndex state.boxScopes)
                  (\s -> snoc s b)
                  state.boxScopes
          }
    )
  where
  lastIndex a = length a - 1

-- | Drop the innermost scope from all scopes.
dropScope :: Validation Unit
dropScope =
  modify_
    ( \s ->
        s
          { lineScopes = unsafePartial $ fromJust $ init s.lineScopes
          , boxScopes = unsafePartial $ fromJust $ init s.boxScopes
          , termScopes = unsafePartial $ fromJust $ init s.termScopes
          }
    )

-- Checks on current state.
-- | Check if line is in scope.
lineInScope :: Int -> ValidationEnv -> Boolean
lineInScope n ve = elem n (concat ve.lineScopes)

-- | Check if box is in scope.
boxInScope :: Tuple Int Int -> ValidationEnv -> Boolean
boxInScope b ve = elem b (concat ve.boxScopes)

-- | Check if term is in scope.
termInScope :: Term -> ValidationEnv -> Boolean
termInScope t ve = elem t (concat ve.termScopes)
