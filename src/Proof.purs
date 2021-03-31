module Proof
  ( NdError(..)
  , Rule(..)
  , ND
  , Proof
  , ProofRow
  , Box
  , runND
  , addProof
  , openBox
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
import Control.Monad.State (State, class MonadState, execState, modify_, get, gets)
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError, except)
import Formula (Formula(..))
import Util

data Rule
  = Premise
  | Assumption
  | AndElim1 Int
  | AndElim2 Int
  | AndIntro Int Int
  | ImplElim Int Int
  | ImplIntro
  | BottomElim
  | DoubleNegElim Int
  | NegElim
  | ModusTollens
  | DoubleNegIntro

derive instance eqRule :: Eq Rule

instance showRule :: Show Rule where
  show Premise = "Premise"
  show Assumption = "Assumption"
  show (AndElim1 _) = "∧e1"
  show (AndElim2 _) = "∧e2"
  show (AndIntro _ _) = "∧i"
  show (ImplElim _ _) = "->e"
  show (ImplIntro) = "->i"
  show (BottomElim) = "Bottom elimination"
  show (DoubleNegElim _) = "Double neg elimination"
  show (NegElim) = "Neg elimination"
  show (ModusTollens) = "MT"
  show (DoubleNegIntro) = "Double neg introduction"

data NdError
  = BadRef
  | RefDiscarded
  | RefOutOfBounds
  | BadRule
  | BadFormula
  | FormulaMismatch
  | InvalidRule

type ProofRow
  = { formula :: Maybe Formula
    , rule :: Maybe Rule
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
    , conclusion :: Maybe Formula
    }

newtype ND a
  = ND (State Proof a)

derive newtype instance functorNd :: Functor ND

derive newtype instance applyNd :: Apply ND

derive newtype instance applicativeNd :: Applicative ND

derive newtype instance bindNd :: Bind ND

derive newtype instance monadNd :: Monad ND

derive newtype instance monadStateNd :: MonadState Proof ND

runND :: forall a. Maybe Formula -> ND a -> Proof
runND conclusion (ND m) =
  execState m
    { rows: []
    , discarded: Set.empty
    , boxes: Nil
    , conclusion
    }

proofRef :: Int -> ExceptT NdError ND Formula
proofRef i = do
  { rows, discarded } <- get
  { formula } <- except $ note RefOutOfBounds $ rows !! (i - 1)
  when (i `Set.member` discarded) $ throwError RefDiscarded
  except $ note BadRef formula

-- | Takes a user-provided formula and ND state and tries to apply the rule.
applyRule :: Rule -> Maybe Formula -> ExceptT NdError ND Formula
applyRule rule formula = case rule of
  Premise -> except $ note BadFormula formula
  Assumption -> except $ note BadFormula formula
  AndElim1 i -> do
    a <- proofRef i
    case a of
      And x _ -> pure x
      _ -> throwError BadRule
  AndElim2 i -> do
    a <- proofRef i
    case a of
      And _ x -> pure x
      _ -> throwError BadRule
  AndIntro i j -> do
    a <- proofRef i
    b <- proofRef j
    pure $ And a b
  ImplElim i j -> do
    a <- proofRef i
    b <- proofRef j
    case a, b of
      Implies x y, z
        | x == z -> pure y
      z, Implies x y
        | x == z -> pure y
      _, _ -> throwError BadRule
  DoubleNegElim i -> do
    a <- proofRef i
    case traceShowId a of
      Not (Not x) -> pure x
      _ -> throwError BadRule
  _ -> throwError BadRule -- TODO remove

addProof :: { formula :: Maybe Formula, rule :: Maybe Rule } -> ND Unit
addProof { formula: inputFormula, rule } = do
  -- Try to apply the rule (and get the correct formula)
  result <- runExceptT $ (except $ note InvalidRule rule) >>= (flip applyRule) inputFormula
  let
    formula = hush result

    error = case inputFormula, result of
      _, Left e -> Just e
      Nothing, Right _ -> Just BadFormula
      Just f, Right g
        | f == g -> Nothing
        | otherwise -> Just FormulaMismatch
  modify_ \proof -> proof { rows = snoc proof.rows { formula, rule, error } }

-- TODO Should only be able to open box on assumption
-- Need to check that two boxes are not opened on each other without formula in between
openBox :: ND Unit
openBox = modify_ \proof -> proof { boxes = { startIdx: Array.length proof.rows } : proof.boxes }

closeBox :: ND Unit
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
