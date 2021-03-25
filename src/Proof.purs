module Proof
  ( NdError
  , Rule
  , Nd
  , emptyProof
  , addProof
  , addBox
  , closeBox
  ) where

import Prelude
import Data.Either (Either(..), note, hush)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Data.Array as Array
import Data.Array (snoc, (!!), (..))
import Data.List as List
import Data.List (List(Nil), (:))
import Data.Set as Set
import Data.Set (Set)
import Control.Monad.State (State, class MonadState, modify_, gets)
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError, except)
import Formula (Formula(..))

data Rule
  = Premise
  | Assumption
  | AndElimE1 Int
  | AndElimE2 Int
  | AndIntro Int Int
  | ImplElim
  | ImplIntro
  | BottomElim
  | DoubleNegElim
  | NegElim
  | ModusTollens
  | DoubleNegIntro

instance showRule :: Show Rule where
  show Premise = "Premise"
  show Assumption = "Assumption"
  show (AndElimE1 _) = "∧e1"
  show (AndElimE2 _) = "∧e2"
  show (AndIntro _ _) = "∧i"
  show (ImplElim) = "->e"
  show (ImplIntro) = "->i"
  show (BottomElim) = "Bottom elimination"
  show (DoubleNegElim) = "Double neg elimination"
  show (NegElim) = "Neg elimination"
  show (ModusTollens) = "MT"
  show (DoubleNegIntro) = "Double neg introduction"

data NdError
  = BadRef
  | RefDiscarded
  | BadRule
  | BadFormula
  | FormulaMismatch

type ProofRow
  = { formula :: Maybe Formula
    , rule :: Rule
    , error :: Maybe NdError
    }

type Box
  = { startIdx :: Int
    -- TODO Store accessible sub-boxes for →i/BottomElim
    }

-- | Partial or completed ND derivation.
type Proof
  = { rows :: Array ProofRow
    , discarded :: Set Int
    , boxes :: List Box -- Stack of nested boxes
    , conclusion :: Formula
    }

newtype Nd a
  = Nd (State Proof a)

derive newtype instance functorNd :: Functor Nd

derive newtype instance applyNd :: Apply Nd

derive newtype instance applicativeNd :: Applicative Nd

derive newtype instance bindNd :: Bind Nd

derive newtype instance monadNd :: Monad Nd

derive newtype instance monadStateNd :: MonadState Proof Nd

emptyProof :: Formula -> Proof
emptyProof conclusion =
  { rows: []
  , discarded: Set.empty
  , boxes: Nil
  , conclusion
  }

proofRef :: Int -> ExceptT NdError Nd (Maybe Formula)
proofRef i = do
  rows <- gets _.rows
  discarded <- gets _.discarded
  { formula } <- except $ note BadRef $ rows !! i
  when (i `Set.member` discarded) $ throwError RefDiscarded
  pure formula

-- TODO Should only be able to open box on assumption
-- Need to check that two boxes are not opened on each other without formula in between
addBox :: Nd Unit
addBox = modify_ \proof -> proof { boxes = { startIdx: Array.length proof.rows } : proof.boxes }

closeBox :: Nd Unit
closeBox = do
  modify_ \proof ->
    let
      { head: box, tail: boxes' } = unsafePartial $ fromJust $ List.uncons proof.boxes

      startIdx = box.startIdx

      endIdx = Array.length proof.rows - 1

      newDiscards = Set.fromFoldable $ startIdx .. endIdx
    in
      proof
        { discarded = proof.discarded <> newDiscards
        , boxes = boxes'
        }

-- | Takes a user-provided formula and ND state and tries to apply the rule.
applyRule :: Rule -> Maybe Formula -> ExceptT NdError Nd Formula
applyRule rule formula = case rule of
  AndElimE1 i -> do
    a <- proofRef i
    case a of
      Just (And x _) -> pure x
      _ -> throwError BadRule
  _ -> unsafeCrashWith "unimplemented"

addProof :: { formula :: Maybe Formula, rule :: Rule } -> Nd Unit
addProof { formula: inputFormula, rule } = do
  -- Try to apply the rule (and get the correct formula)
  result <- runExceptT $ applyRule rule inputFormula
  let
    formula = hush result

    error = case inputFormula, result of
      _, Left e -> Just e
      Nothing, Right _ -> Just BadFormula
      Just f, Right g
        | f == g -> Nothing
        | otherwise -> Just FormulaMismatch
  modify_ \proof -> proof { rows = snoc proof.rows { formula, rule, error } }
