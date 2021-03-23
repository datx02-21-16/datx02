module Proof (NdError,
              Rule,
              Nd,
              proofRef,
              addProof,
              addBox,
              closeBox) where

import Prelude
import Control.Monad.State (State, modify_, gets)
import Data.Array (snoc, (!!))
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Data.Array as Array
import Data.List as List
import Data.List (List(Nil), (:))
import Data.Set as Set
import Data.Set (Set)

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

--data RuleApp = {rule :: Rule , formulas :: Array Formula}
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

data NdError = BadRef | RefDiscarded | BadRule | BadFormula | FormulaMismatch

type Proof = { formula :: Maybe Formula
             , rule :: Rule
             , error :: Maybe NdError
             }

type Box = { startIdx :: Int
             -- TODO Store accessible sub-boxes for →i/BottomElim
           }

-- | Partial or completed ND derivation.
newtype Nd = Nd { proofs :: Array Proof
                , discarded :: Set Int
                , boxes :: List Box -- Stack of nested boxes
                , consequent :: Formula
                }

newNd :: Array Formula -> Formula -> Nd
newNd premises consequent
  = Nd { proofs: ({ formula: _
                  , rule: Premise
                  , error: Nothing } <<< Just) <$> premises
       , discarded: Set.empty
       , boxes: Nil
       , consequent
       }

proofRef :: Int -> Nd -> Either NdError (Maybe Formula)
proofRef i (Nd { proofs, discarded }) = do
  { formula } <- note BadRef $ proofs !! i
  when (i `Set.member` discarded) $ Left RefDiscarded
  pure formula

-- TODO Should only be able to open box on assumption
addBox :: Nd -> Nd
addBox (Nd nd@{ proofs, boxes })
  = Nd $ nd { boxes = { startIdx: Array.length proofs }:boxes }

closeBox :: Nd -> Nd
closeBox (Nd nd@{ proofs, discarded, boxes })
  = Nd $ nd { discarded = discarded <> newDiscards
            , boxes = boxes' }
  where
    { head: box, tail: boxes' } = unsafePartial $ fromJust
        $ List.uncons boxes
    startIdx = box.startIdx
    endIdx = Array.length proofs
    newDiscards = Set.fromFoldable $ Array.range startIdx endIdx

-- | Takes a user-provided formula and ND state and tries to apply the rule.
applyRule :: Rule -> Maybe Formula -> Nd -> Either NdError Formula
applyRule rule formula nd = case rule of
  AndElimE1 i -> do
    a <- proofRef i nd
    case a of
      Just (And x _) -> pure x
      _ -> Left BadRule
  _ -> unsafeCrashWith "unimplemented"

addProof :: Maybe Formula -> Rule -> Nd -> Nd
addProof formula rule (Nd nd@{ proofs }) = let
  ruleResult = do
    ruleFormula <- applyRule rule formula (Nd nd)
    case formula of
      Nothing -> Left BadFormula
      Just f | f /= ruleFormula -> Left FormulaMismatch
      _ -> pure ruleFormula
  error = case ruleResult of
    Left x -> Just x
    _ -> Nothing
  in Nd $ nd { proofs = snoc proofs { formula, rule, error } }
