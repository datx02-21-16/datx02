module Proof
  ( NdError(..)
  , Rule(..)
  , ND
  , Proof
  , ProofRow
  , Box
  , Scope
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
import Data.Array (snoc, (!!))
import Data.Array as Array
import Data.Either (Either(..), note, hush)
import Data.Foldable (all, any)
import Data.Maybe (Maybe(..), isJust, fromJust)
import Data.Tuple (Tuple(..))
import Formula (Formula(..), Variable, bottom)
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
  = Tuple Int Int

type Scope
  = { lines :: Array Int
    , boxes :: Array Box
    , vars :: Array Variable
    }

emptyScope :: Scope
emptyScope =
  { lines: []
  , boxes: []
  , vars: []
  }

-- | Partial or completed ND derivation.
type Proof
  = { rows :: Array ProofRow
    , boxStarts :: Array Int
    , scopes :: Array Scope
    }

isPremise :: ProofRow -> Boolean
isPremise r = r.rule == Just Premise

newtype ND a
  = ND (State Proof a)

a :: Box -> Box -> Boolean
a b1 b2 = b1 == b2

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
    , boxStarts: []
    , scopes: [ emptyScope ]
    }

  checkCompleteness =
    isJust
      <$> runMaybeT do
          { rows, boxStarts, scopes } <- get
          -- Check if there are unclosed boxes
          unless (boxStarts == []) $ MaybeT (pure Nothing)
          -- Should be no errors
          when (any (isJust <<< _.error) rows) $ MaybeT (pure Nothing)
          -- The last row should equal the conclusion in a complete proof
          lastRow <- MaybeT $ pure $ (Array.last rows) >>= _.formula
          unless (Just lastRow == conclusion) $ MaybeT (pure Nothing)

-- | Get the formula at the given one-based row index, if it is in scope.
proofRef :: Int -> ExceptT NdError ND Formula
proofRef i = do
  { rows, boxStarts, scopes } <- get
  { formula, error } <- except $ note RefOutOfBounds $ rows !! (i - 1)
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
applyRule rule formula = do
  { rows, boxStarts, scopes } <- get
  case rule of
    Premise -> do
      if all isPremise rows then except $ note BadFormula formula else throwError BadRule
    -- TODO Check if this assumption is the first formula in the current box
    Assumption -> except $ note BadFormula formula
    AndElim1 i -> do
      when (not lineInScope i scopes) $ throwError RefDiscarded
      a <- proofRef i
      case a of
        And x _ -> pure x
        _ -> throwError BadRule
    AndElim2 i -> do
      when (not lineInScope i scopes) $ throwError RefDiscarded
      a <- proofRef i
      case a of
        And _ x -> pure x
        _ -> throwError BadRule
    AndIntro i j -> do
      when (not $ lineInScope i scopes && lineInScope j scopes) $ throwError RefDiscarded
      a <- proofRef i
      b <- proofRef j
      pure $ And a b
    OrElim i box1@(Tuple j1 j2) box2@(Tuple k1 k2) -> do
      when (not $ lineInScope i scopes && boxInScope box1 scopes && boxInScope box2 scopes) $ throwError RefDiscarded
      a <- proofRef i
      b1 <- proofRef j1
      b2 <- proofRef j2
      c1 <- proofRef k1
      c2 <- proofRef k2
      case a of
        Or f1 f2 -> if f1 == b1 && f2 == c1 && b2 == c2 then pure b2 else throwError BadRule
        _ -> throwError BadRule
    OrIntro1 i -> do
      when (not lineInScope i scopes) $ throwError RefDiscarded
      case formula of
        Just f@(Or f1 _) -> do
          a <- proofRef i
          if a == f1 then pure f else throwError BadFormula
        _ -> throwError BadRule
    OrIntro2 i -> do
      when (not lineInScope i scopes) $ throwError RefDiscarded
      case formula of
        Just f@(Or _ f2) -> do
          a <- proofRef i
          if a == f2 then pure f else throwError BadFormula
        _ -> throwError BadRule
    ImplElim i j -> do
      when (not $ lineInScope i scopes && lineInScope j scopes) $ throwError RefDiscarded
      a <- proofRef i
      b <- proofRef j
      case a, b of
        Implies x y, z
          | x == z -> pure y
        z, Implies x y
          | x == z -> pure y
        _, _ -> throwError BadRule
    ImplIntro box@(Tuple i j) -> do
      when (not boxInScope box scopes) $ throwError RefDiscarded
      a <- proofRef i
      b <- proofRef j
      pure $ Implies a b
    NegElim i j -> do
      when (not $ lineInScope i scopes && lineInScope j scopes) $ throwError RefDiscarded
      a <- proofRef i
      b <- proofRef j
      if a == Not b || Not a == b then pure bottom else throwError BadRule
    NegIntro box@(Tuple i j) -> do
      when (not boxInScope box scopes) $ throwError RefDiscarded
      a <- proofRef i
      b <- proofRef j
      if b == bottom then pure $ Not a else throwError BadRule
    BottomElim i -> do
      when (not lineInScope i scopes) $ throwError RefDiscarded
      a <- proofRef i
      if a == bottom then except $ note BadFormula formula else throwError BadRule
    DoubleNegElim i -> do
      when (not lineInScope i scopes) $ throwError RefDiscarded
      a <- proofRef i
      case a of
        Not (Not x) -> pure x
        _ -> throwError BadRule
    ModusTollens i j -> throwError BadRule
    DoubleNegIntro i -> do
      when (not lineInScope i scopes) $ throwError RefDiscarded
      (Not <<< Not) <$> proofRef i
    PBC box@(Tuple i j) -> do
      when (not boxInScope box scopes) $ throwError RefDiscarded
      a <- proofRef i
      b <- proofRef j
      case a, b of
        Not f, bottom -> pure f
        _, _ -> throwError BadRule
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
  modify_ \proof ->
    proof
      { rows = snoc proof.rows { formula, rule, error }
      , scopes = addLineToInnermost (Array.length proof.rows + 1) proof.scopes
      }

openBox :: ND Unit
openBox =
  modify_ \proof ->
    proof
      { boxStarts = (Array.length proof.rows + 1) `Array.cons` proof.boxStarts
      , scopes = emptyScope `Array.cons` proof.scopes
      }

-- | Close the innermost currently open box.
-- |
-- | Panics if there is no such box.
closeBox :: ND Unit
closeBox = do
  modify_ \proof ->
    let
      { head: startOfJustClosed, tail: stillOpen } = unsafePartial $ fromJust $ Array.uncons proof.boxStarts

      endOfJustClosed = Array.length proof.rows

      justClosed = Tuple startOfJustClosed endOfJustClosed
    in
      proof
        { boxStarts = stillOpen
        , scopes = addBoxToInnermost justClosed $ unsafePartial $ fromJust $ Array.tail proof.scopes
        }

boxInScope :: Box -> Array Scope -> Boolean
boxInScope b ss = any (\s -> b `Array.elem` s.boxes) ss

lineInScope :: Int -> Array Scope -> Boolean
lineInScope l ss = any (\s -> l `Array.elem` s.lines) ss

varInScope :: Variable -> Array Scope -> Boolean
varInScope v ss = any (\s -> v `Array.elem` s.vars) ss

addBox :: Box -> Scope -> Scope
addBox b s = s { boxes = b `Array.cons` s.boxes }

addLine :: Int -> Scope -> Scope
addLine l s = s { lines = l `Array.cons` s.lines }

addVar :: Variable -> Scope -> Scope
addVar v s = s { vars = v `Array.cons` s.vars }

addBoxToInnermost :: Box -> Array Scope -> Array Scope
addBoxToInnermost b ss = addBox b innermost `Array.cons` outerScopes
  where
  { head: innermost, tail: outerScopes } = unsafePartial $ fromJust $ Array.uncons ss

addLineToInnermost :: Int -> Array Scope -> Array Scope
addLineToInnermost l ss = addLine l innermost `Array.cons` outerScopes
  where
  { head: innermost, tail: outerScopes } = unsafePartial $ fromJust $ Array.uncons ss

addVarToInnermost :: Variable -> Array Scope -> Array Scope
addVarToInnermost v ss = addVar v innermost `Array.cons` outerScopes
  where
  { head: innermost, tail: outerScopes } = unsafePartial $ fromJust $ Array.uncons ss
