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
import Data.Array as Array
import Data.Either (Either(..), note, hush)
import Data.Foldable (all, any)
import Data.List (List(Nil), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Formula
  ( Formula(..)
  , Term(..)
  , allVarsInFormula
  , almostEqual
  , bottomProp
  , equalityProp
  , equivalent
  , hasSingleSubOf
  )
import FormulaOrVar (FFC(FC, VC))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

data Rule
  = Premise
  | Assumption
  | AndElim1 (Maybe Int)
  | AndElim2 (Maybe Int)
  | AndIntro (Maybe Int) (Maybe Int)
  | OrElim (Maybe Int) (Maybe Box) (Maybe Box)
  | OrIntro1 (Maybe Int)
  | OrIntro2 (Maybe Int)
  | ImplElim (Maybe Int) (Maybe Int)
  | ImplIntro (Maybe Box)
  | NegElim (Maybe Int) (Maybe Int)
  | NegIntro (Maybe Box)
  | BottomElim (Maybe Int)
  | DoubleNegElim (Maybe Int)
  | ModusTollens (Maybe Int) (Maybe Int)
  | DoubleNegIntro (Maybe Int)
  | PBC (Maybe Box)
  | LEM
  | Copy (Maybe Int)
  | Fresh
  | ForallElim (Maybe Int)
  | ForallIntro (Maybe Box)
  | ExistsElim (Maybe Int) (Maybe Box)
  | ExistsIntro (Maybe Int)
  | EqElim (Maybe Int) (Maybe Int)
  | EqIntro

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
  show (Copy _) = "Copy"
  show Fresh = "Fresh"
  show (ForallElim _) = "∀e"
  show (ForallIntro _) = "∀i"
  show (ExistsElim _ _) = "∃e"
  show (ExistsIntro _) = "∃i"
  show (EqElim _ _) = "=e"
  show EqIntro = "=i"

data NdError
  = BadRef
  | RefDiscarded
  | RefOutOfBounds
  | BadRule
  | BadFormula
  | FormulaMismatch
  | InvalidRule
  | NotABox
  | VarExists

instance showNdError :: Show NdError where
  show BadRef = "bad reference"
  show RefDiscarded = "reference discarded"
  show RefOutOfBounds = "reference out of bounds"
  show BadRule = "bad rule"
  show BadFormula = "bad formula"
  show FormulaMismatch = "formula mismatch"
  show InvalidRule = "invalid rule"
  show NotABox = "not a box"
  show VarExists = "variable already exists"

derive instance eqNdError :: Eq NdError

type ProofRow
  = { formula :: Maybe FFC
    , rule :: Maybe Rule
    , error :: Maybe NdError
    }

type Box
  = Tuple Int Int

-- | A scope which contains the lines, boxes and vars available.
type Scope
  = { lines :: Set Int
    , boxes :: Array Box
    , boxStart :: Maybe Int
    }

-- | Empty scope
fullScope :: Scope
fullScope =
  { lines: Set.empty
  , boxes: []
  , boxStart: Nothing
  }

newScope :: Int -> Scope
newScope n =
  { lines: Set.empty
  , boxes: []
  , boxStart: Just n
  }

-- | Partial or completed ND derivation.
type Proof
  = { rows :: Array ProofRow
    , scopes :: List Scope
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
runND :: forall a. Maybe FFC -> ND a -> Tuple Boolean Proof
runND conclusion (ND nd) = runState (nd *> checkCompleteness) initialState
  where
  initialState =
    { rows: []
    , scopes: fullScope : Nil
    }

  checkCompleteness =
    isJust
      <$> runMaybeT do
          { rows, scopes } <- get
          -- Check if there are unclosed boxes
          unless (isNothing $ innerBoxStart scopes) $ MaybeT (pure Nothing)
          -- Should be no errors
          when (any (isJust <<< _.error) rows) $ MaybeT (pure Nothing)
          -- The last row should equal the conclusion in a complete proof
          lastRow <- MaybeT $ pure $ (Array.last rows) >>= _.formula
          unless (Just lastRow == conclusion) $ MaybeT (pure Nothing)

innerBoxStart :: List Scope -> Maybe Int
innerBoxStart ss = (\s -> s.boxStart) $ unsafePartial $ fromJust $ List.head ss

-- | Get the formula at the given one-based row index, if it is in scope.
proofRef :: Maybe Int -> ExceptT NdError ND FFC
proofRef ref = do
  i <- except $ note BadRef ref
  { rows, scopes } <- get
  { formula, error } <- except $ note RefOutOfBounds $ rows Array.!! (i - 1)
  when (not (lineInScope i scopes)) $ throwError RefDiscarded
  when (error == Just BadFormula) $ throwError BadRef -- User needs to have input the formula
  except $ note BadRef formula

boxRef :: Maybe Box -> ExceptT NdError ND (Tuple FFC FFC)
boxRef ref = do
  box@(Tuple i j) <- except $ note BadRef ref
  { rows, scopes } <- get
  { formula: maybeF1, error: e1 } <- except $ note RefOutOfBounds $ rows Array.!! (i - 1)
  { formula: maybeF2, error: e2 } <- except $ note RefOutOfBounds $ rows Array.!! (j - 1)
  when (isNotBox box scopes) $ throwError NotABox
  when (not (boxInScope box scopes)) $ throwError RefDiscarded
  when (e1 == Just BadFormula || e2 == Just BadFormula) $ throwError BadRef -- User needs to have input the formula
  let
    maybeBox = case maybeF1, maybeF2 of
      Just f1, Just f2 -> Just $ Tuple f1 f2
      _, _ -> Nothing
  except $ note BadRef maybeBox

-- | Attempt to apply the specified rule given the user-provided formula.
-- |
-- | Does not modify state.
-- |
-- | The formula inputted by the user is needed to uniformly handle
-- | rules such as LEM, which violate the subformula property. They
-- | can then return that formula as the result, provided it is valid.
applyRule :: Rule -> Maybe FFC -> ExceptT NdError ND FFC
applyRule rule formula = if isJust formula then applyRule' else throwError BadFormula
  where
  applyRule' = do
    { rows, scopes } <- get
    case rule of
      Premise -> do
        case formula of
          Just (FC _) ->
            if all isPremise rows then
              except $ note BadFormula formula
            else
              throwError BadRule
          _ -> throwError FormulaMismatch
      -- TODO Check if this assumption is the first formula in the current box
      Assumption -> do except $ note BadFormula formula
      AndElim1 i -> do
        a <- proofRef i
        case a of
          FC (And x _) -> pure $ FC x
          _ -> throwError BadRule
      AndElim2 i -> do
        a <- proofRef i
        case a of
          FC (And _ x) -> pure $ FC x
          _ -> throwError BadRule
      AndIntro i j -> do
        a <- proofRef i
        b <- proofRef j
        case a, b of
          FC f1, FC f2 -> pure $ FC (And f1 f2)
          _, _ -> throwError BadRule
      OrElim i box1 box2 -> do
        a <- proofRef i
        (Tuple b1 b2) <- boxRef box1
        (Tuple c1 c2) <- boxRef box2
        case a, b1, c1, b2, c2 of
          FC (Or f1 f2), FC b1', FC c1', FC b2', FC c2' ->
            if f1 `equivalent` b1' && f2 `equivalent` c1' && b2' `equivalent` c2' then
              pure b2
            else
              throwError BadRule
          _, _, _, _, _ -> throwError BadRule
      OrIntro1 i -> do
        case formula of
          Just f@(FC f'@(Or f1 _)) -> do
            a <- proofRef i
            case a of
              FC a' -> if a' `equivalent` f1 then pure f else throwError FormulaMismatch
              _ -> throwError BadFormula
          _ -> throwError BadRule
      OrIntro2 i -> do
        case formula of
          Just f@(FC f'@(Or _ f2)) -> do
            a <- proofRef i
            case a of
              FC a' -> if a' `equivalent` f2 then pure f else throwError FormulaMismatch
              _ -> throwError BadRef
          _ -> throwError BadRule
      ImplElim i j -> do
        a <- proofRef i
        b <- proofRef j
        case a, b of
          FC (Implies x y), FC z
            | x `equivalent` z -> pure $ FC y
          FC z, FC (Implies x y)
            | x `equivalent` z -> pure $ FC y
          _, _ -> throwError BadRule
      ImplIntro box -> do
        (Tuple a b) <- boxRef box
        case a, b of
          FC f1, FC f2 -> pure $ FC $ Implies f1 f2
          _, _ -> throwError BadRule
      NegElim i j -> do
        a <- proofRef i
        b <- proofRef j
        case a, b of
          FC f1, FC f2 ->
            if f1 `equivalent` Not f2 || Not f1 `equivalent` f2 then
              pure (FC bottomProp)
            else
              throwError BadRule
          _, _ -> throwError BadRule
      NegIntro box -> do
        (Tuple a b) <- boxRef box
        case a, b of
          FC f1, FC f2 -> if f2 == bottomProp then pure $ FC (Not f1) else throwError BadRule
          _, _ -> throwError BadRule
      BottomElim i -> do
        a <- proofRef i
        if a == FC bottomProp then except $ note BadFormula formula else throwError BadRule
      DoubleNegElim i -> do
        a <- proofRef i
        case a of
          FC (Not (Not x)) -> pure $ FC x
          _ -> throwError BadRule
      ModusTollens i j -> do
        a <- proofRef i
        b <- proofRef j
        case a, b of
          FC (Implies x y), FC (Not z) -> if y `equivalent` z then pure (FC $ Not x) else throwError BadRule
          FC (Not z), FC (Implies x y) -> if y `equivalent` z then pure (FC $ Not x) else throwError BadRule
          _, _ -> throwError BadRule
      DoubleNegIntro i -> do
        a <- proofRef i
        case a of
          FC f -> pure $ FC $ Not $ Not f
          _ -> throwError BadFormula
      PBC box -> do
        (Tuple a b) <- boxRef box
        case a, b of
          FC (Not f), FC f2 -> if f2 == bottomProp then pure $ FC f else throwError BadRule
          _, _ -> throwError BadRule
      LEM -> case formula of
        Just f@(FC (Or f1 f2))
          | f1 `equivalent` Not f2 || f2 `equivalent` Not f1 -> pure f
        _ -> throwError BadRule
      Copy i -> do
        a <- proofRef i
        case a of
          FC _ -> pure a
          _ -> throwError BadRule
      Fresh -> case formula of
        Just vc@(VC v) -> pure vc
        _ -> throwError FormulaMismatch
      ForallElim i -> case formula of
        Just formula'@(FC fTarget) -> do
          f <- proofRef i
          case f of
            FC (Forall v f') ->
              if isJust $ hasSingleSubOf v f' fTarget then
                pure formula'
              else
                throwError FormulaMismatch
            _ -> throwError BadRule
        _ -> throwError FormulaMismatch
      ForallIntro box -> case formula of
        Just formula'@(FC fTarget@(Forall v f)) -> do
          (Tuple a b) <- boxRef box
          case a, b of
            VC vLocal, FC fLocal -> case hasSingleSubOf v f fLocal of
              Just (Var vSub) -> if vSub == vLocal then pure formula' else throwError BadRule
              _ -> throwError BadRule
            _, _ -> throwError BadRule
        _ -> throwError FormulaMismatch
      ExistsElim i box -> case formula of
        Just formula'@(FC fTarget) -> do
          a <- proofRef i
          (Tuple b1 b2) <- boxRef box
          case a, b1, b2 of
            FC (Exists v f), FC f', FC b2' -> do
              case hasSingleSubOf v f f' of
                Just (Var intro) -> do
                  when (intro `Array.elem` allVarsInFormula fTarget) (throwError BadRule)
                  if b2' `equivalent` fTarget then pure formula' else throwError FormulaMismatch
                _ -> throwError BadRule
            _, _, _ -> throwError FormulaMismatch
        _ -> throwError FormulaMismatch
      ExistsIntro i -> case formula of
        Just formula'@(FC (Exists v fTarget)) -> do
          a <- proofRef i
          case a of
            FC fLocal ->
              if isJust $ hasSingleSubOf v fTarget fLocal then
                pure formula'
              else
                throwError BadRule
            _ -> throwError BadRule
        _ -> throwError FormulaMismatch
      EqElim i j -> case formula of
        Just formula'@(FC verifyF) -> do
          eqFormula <- proofRef i
          subFormula <- proofRef j
          case eqFormula, subFormula of
            FC eqF@(Predicate _ [ t1, t2 ]), FC sF ->
              if (eqF == equalityProp t1 t2 && almostEqual t1 t2 sF verifyF) then
                pure formula'
              else
                throwError FormulaMismatch
            _, _ -> throwError BadFormula
        _ -> throwError FormulaMismatch
      EqIntro -> case formula of
        Just formula'@(FC p@(Predicate _ [ t1, t2 ])) ->
          if p == equalityProp t1 t2 && t1 == t2 then
            pure formula'
          else
            throwError FormulaMismatch
        _ -> throwError FormulaMismatch

-- | Add a row to the derivation.
-- |
-- | Possibly an error will be attached. If the user has not
-- | inputted the formula, then the correct formula from the rule
-- | application will be used in its stead, if possible. This can be
-- | used to generate a formula from use of some rule.
addProof :: { formula :: Maybe FFC, rule :: Maybe Rule } -> ND Unit
addProof { formula: inputFormula, rule } = do
  -- Try to apply the rule (and get the correct formula)
  result <- runExceptT $ (except $ note InvalidRule rule) >>= (flip applyRule) inputFormula
  { scopes } <- get
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
      { rows = Array.snoc proof.rows { formula, rule, error }
      , scopes = addLineToInnermost (Array.length proof.rows + 1) proof.scopes
      }
  where
  scopedVars f = case f of
    Not f' -> scopedVars f'
    And f' f'' -> Array.nub $ scopedVars f' <> scopedVars f''
    Or f' f'' -> Array.nub $ scopedVars f' <> scopedVars f''
    Implies f' f'' -> Array.nub $ scopedVars f' <> scopedVars f''
    Forall v f' -> Array.delete v $ scopedVars f'
    Exists v f' -> Array.delete v $ scopedVars f'
    _ -> Array.nub $ allVarsInFormula f

-- | Open a new box.
openBox :: ND Unit
openBox =
  modify_ \proof ->
    proof { scopes = (newScope $ Array.length proof.rows + 1) : proof.scopes }

-- | Close the innermost currently open box.
-- |
-- | Panics if there is no such box.
closeBox :: ND Unit
closeBox = do
  modify_ \proof ->
    let
      boxStart = unsafePartial $ fromJust $ innerBoxStart $ proof.scopes

      boxEnd = Array.length proof.rows

      justClosed = Tuple boxStart boxEnd
    in
      proof
        { scopes = addBoxToInnermost justClosed $ unsafePartial $ fromJust $ List.tail proof.scopes
        }

-- | Check if a proof row is a Premise.
isPremise :: ProofRow -> Boolean
isPremise r = r.rule == Just Premise

-- | Check if a box is really a box.
isNotBox :: Box -> List Scope -> Boolean
isNotBox b@(Tuple l1 l2) ss = lineInScope l1 ss && lineInScope l2 ss && not (boxInScope b ss)

-- | Check if a box is in scope in a stack of scopes.
boxInScope :: Box -> List Scope -> Boolean
boxInScope b ss = any (\s -> b `Array.elem` s.boxes) ss

-- | Check if a line is in scope in a stack of scopes.
lineInScope :: Int -> List Scope -> Boolean
lineInScope l ss = any (\s -> l `Set.member` s.lines) ss

-- | Add a box to a scope.
addBox :: Box -> Scope -> Scope
addBox b s = s { boxes = b `Array.cons` s.boxes }

-- | Add a line to a scope.
addLine :: Int -> Scope -> Scope
addLine l s = s { lines = l `Set.insert` s.lines }

-- | Add a box to the innermost scope in a stack of scopes.
addBoxToInnermost :: Box -> List Scope -> List Scope
addBoxToInnermost b (innermost : outerScopes) = addBox b innermost : outerScopes

addBoxToInnermost _ _ = unsafeCrashWith "Cannot add to an empty list of scopes."

-- | Add a line to the innermost scope in a stack of scopes.
addLineToInnermost :: Int -> List Scope -> List Scope
addLineToInnermost l (innermost : outerScopes) = addLine l innermost : outerScopes

addLineToInnermost _ _ = unsafeCrashWith "Cannot add to an empty list of scopes."
