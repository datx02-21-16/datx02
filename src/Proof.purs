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
import Control.Alt ((<|>))
import Control.Monad.Except.Trans (ExceptT, except, runExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.State (State, class MonadState, runState, modify_, get)
import Data.Array (snoc, (!!), (..))
import Data.Array as Array
import Data.Either (Either(..), note, hush)
import Data.Foldable (any)
import Data.List (List(Nil), (:))
import Data.List as List
import Data.Maybe (Maybe(..), isJust, fromJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple)
import Formula (Formula(..), bottom)
import Partial.Unsafe (unsafePartial)

data Rule
  = Premise
  | Assumption
  | AndElim1 Int
  | AndElim2 Int
  | AndIntro Int Int
  | OrElim Int (Tuple Int Int) (Tuple Int Int)
  | OrIntro1 Int
  | OrIntro2 Int
  | ImplElim Int Int
  | ImplIntro (Tuple Int Int)
  | NegElim Int Int
  | NegIntro (Tuple Int Int)
  | BottomElim Int
  | DoubleNegElim Int
  | ModusTollens Int Int
  | DoubleNegIntro Int
  | PBC (Tuple Int Int)
  | LEM

derive instance eqRule :: Eq Rule

instance showRule :: Show Rule where
  show Premise = "Premise"
  show Assumption = "Assumption"
  show (AndElim1 _) = "∧e1"
  show (AndElim2 _) = "∧e2"
  show (AndIntro _ _) = "∧i"
  show (OrElim _ _ _) = "∨e"
  show (OrIntro1 _) = "∨i1"
  show (OrIntro2 _) = "∨i2"
  show (ImplElim _ _) = "→e"
  show (ImplIntro _) = "→i"
  show (NegElim _ _) = "Neg elimination"
  show (NegIntro _) = "Neg intro"
  show (BottomElim _) = "Bottom elimination"
  show (DoubleNegElim _) = "Double neg elimination"
  show (ModusTollens _ _) = "MT"
  show (DoubleNegIntro _) = "Double neg introduction"
  show (PBC _) = "Proof by contradiction"
  show LEM = "Law of excluded middle"

data NdError
  = BadRef
  | RefDiscarded
  | RefOutOfBounds
  | BadRule
  | BadFormula
  | FormulaMismatch
  | InvalidRule

derive instance eqNdError :: Eq NdError

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
    }

newtype ND a
  = ND (State Proof a)

derive newtype instance functorND :: Functor ND

derive newtype instance applyND :: Apply ND

derive newtype instance applicativeND :: Applicative ND

derive newtype instance bindND :: Bind ND

derive newtype instance monadND :: Monad ND

derive newtype instance monadStateND :: MonadState Proof ND

-- | Verifies whether the ND derivation correctly proves the specified conclusion.
-- |
-- | Returns the completeness status, together with the proof as given
-- | annotated with any potential errors.
runND :: forall a. Maybe Formula -> ND a -> Tuple Boolean Proof
runND conclusion (ND nd) = runState (nd *> checkCompleteness) initialState
  where
  initialState =
    { rows: []
    , discarded: Set.empty
    , boxes: Nil
    }

  checkCompleteness =
    isJust
      <$> runMaybeT do
          { rows, boxes } <- get
          -- Check if there are unclosed boxes
          unless (boxes == Nil) $ MaybeT (pure Nothing)
          -- Should be no errors
          when (any (isJust <<< _.error) rows) $ MaybeT (pure Nothing)
          -- The last row should equal the conclusion in a complete proof
          lastRow <- MaybeT $ pure $ (Array.last rows) >>= _.formula
          unless (Just lastRow == conclusion) $ MaybeT (pure Nothing)

-- | Get the formula at the given one-based row index, if it is in scope.
proofRef :: Int -> ExceptT NdError ND Formula
proofRef i = do
  { rows, discarded } <- get
  { formula, error } <- except $ note RefOutOfBounds $ rows !! (i - 1)
  when ((i - 1) `Set.member` discarded) $ throwError RefDiscarded
  when (error == Just BadFormula) $ throwError BadRef -- User needs to have input the formula
  except $ note BadRef formula

-- | Attempt to apply the specified rule given the user-provided formula.
-- |
-- | Does not modify state.
-- |
-- | The formula inputted by the user is needed to uniformly handle
-- | rules such as LEM, which violate the subformula property. They
-- | can then return that formula as the result, provided it is valid.
applyRule :: Rule -> Maybe Formula -> ExceptT NdError ND Formula
applyRule rule formula = case rule of
  Premise -> except $ note BadFormula formula
  -- TODO Check if this assumption is the first formula in the current box
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
  OrElim _ _ _ -> throwError BadRule
  OrIntro1 i -> case formula of
    Just f@(Or f1 _) -> do
      a <- proofRef i
      if a == f1 then pure f else throwError BadFormula
    _ -> throwError BadRule
  OrIntro2 i -> case formula of
    Just f@(Or _ f2) -> do
      a <- proofRef i
      if a == f2 then pure f else throwError BadFormula
    _ -> throwError BadRule
  ImplElim i j -> do
    a <- proofRef i
    b <- proofRef j
    case a, b of
      Implies x y, z
        | x == z -> pure y
      z, Implies x y
        | x == z -> pure y
      _, _ -> throwError BadRule
  ImplIntro _ -> throwError BadRule
  NegElim i j -> do
    a <- proofRef i
    b <- proofRef j
    if a == Not b || Not a == b then pure bottom else throwError BadRule
  NegIntro _ -> throwError BadRule
  BottomElim i -> do
    a <- proofRef i
    if a == bottom then except $ note BadFormula formula else throwError BadRule
  DoubleNegElim i -> do
    a <- proofRef i
    case a of
      Not (Not x) -> pure x
      _ -> throwError BadRule
  ModusTollens i j -> throwError BadRule
  DoubleNegIntro i -> (Not <<< Not) <$> proofRef i
  PBC _ -> throwError BadRule
  LEM -> case formula of
    Just f@(Or f1 f2)
      | f1 == Not f2 || f2 == Not f1 -> pure f
    _ -> throwError BadRule

-- | Add a row to the derivation.
-- |
-- | Possibly an error will be attached. If the user has not
-- | inputted the formula, then the correct formula from the rule
-- | application will be used in its stead, if possible. This can be
-- | used to generate a formula from use of some rule.
addProof :: { formula :: Maybe Formula, rule :: Maybe Rule } -> ND Unit
addProof { formula: inputFormula, rule } = do
  -- Try to apply the rule (and get the correct formula)
  result <- runExceptT $ (except $ note InvalidRule rule) >>= (flip applyRule) inputFormula
  let
    formula = inputFormula <|> hush result

    error = case inputFormula, result of
      _, Left e -> Just e
      Nothing, Right _ -> Just BadFormula
      Just f, Right g
        | f == g -> Nothing
        | otherwise -> Just FormulaMismatch
  modify_ \proof -> proof { rows = snoc proof.rows { formula, rule, error } }

openBox :: ND Unit
openBox = modify_ \proof -> proof { boxes = { startIdx: Array.length proof.rows } : proof.boxes }

-- | Close the innermost currently open box.
-- |
-- | Panics if there is no such box.
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
