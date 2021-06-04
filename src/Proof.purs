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
  , MismatchError(..)
  , ArgumentError(..)
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except.Trans (ExceptT, except, runExceptT, throwError)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT)
import Control.Monad.State (State, class MonadState, runState, modify_, get)
import Data.Array as Array
import Data.Array ((!!))
import Data.Either (Either(..), note, hush)
import Data.Foldable (findMap, all, any)
import Data.List (List(Nil), (:))
import Data.List as List
import Data.Maybe (Maybe(..), maybe, fromJust, isJust, isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Formula
  ( Formula(..)
  , Variable
  , Term(..)
  , freeVarsIn
  , almostEqual
  , bottomProp
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
  | BadRef_Box
  | RefDiscarded
  | RefOutOfBounds Int
  | BadRule
  | BadFormula
  | FormulaMismatch MismatchError
  | InvalidRule
  | NotABox
  | InvalidArg ArgumentError
  | BadPremise
  | OccursOutsideBox Variable
  | NotAFresh
  | FreshShadowsVar Variable Int

--TODO: Add error constructors for predicate logic with specific error scenario messages.
data MismatchError
  = BadLem
  | BadOrI_Order
  | BadOrI_Formula
  | GenericMismatch String
  | PremiseM
  | FreshM
  | NotAFormulaM
  | UnexplainedError

data ArgumentError
  = BadAndE
  | BadMt1
  | BadMt2
  | BadImplE
  | BadNegE
  | BadBottomE
  | BadDoubleNegE
  | BadOrE1
  | BadOrE2
  | BadNegI
  | BadPBC
  | BadEq Formula
  | ArgNotFormula

instance showNdError :: Show NdError where
  show BadRef = "bad reference"
  show BadRef_Box = "bad reference box"
  show RefDiscarded = "reference discarded"
  show (RefOutOfBounds i) = "reference out of bounds: " <> show i
  show BadRule = "bad rule"
  show BadFormula = "bad formula"
  show (FormulaMismatch _) = "formula mismatch"
  show InvalidRule = "invalid rule"
  show (InvalidArg _) = "invalid arg"
  show NotABox = "not a box"
  show BadPremise = "Bad premise order"
  show (OccursOutsideBox x) = show x <> " occurs outside its box"
  show NotAFresh = "not a fresh"
  show (FreshShadowsVar v i) = "fresh shadows var " <> show v <> " " <> show i

derive instance eqNdError :: Eq NdError

derive instance eqMismatchError :: Eq MismatchError

derive instance eqArgumentError :: Eq ArgumentError

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
    -- Map of variables to the row indices where they are first introduced
    , vars :: Map Variable Int
    }

-- | Empty scope
fullScope :: Scope
fullScope =
  { lines: Set.empty
  , boxes: []
  , boxStart: Nothing
  , vars: Map.empty
  }

newScope :: Int -> Scope
newScope n =
  { lines: Set.empty
  , boxes: []
  , boxStart: Just n
  , vars: Map.empty
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
runND :: forall a. Maybe Formula -> ND a -> Tuple Boolean Proof
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
          unless (Just lastRow == (FC <$> conclusion)) $ MaybeT (pure Nothing)

innerBoxStart :: List Scope -> Maybe Int
innerBoxStart ss = (\s -> s.boxStart) $ unsafePartial $ fromJust $ List.head ss

-- | Get the formula at the given one-based row index, if it is in scope.
proofRef :: Maybe Int -> ExceptT NdError ND Formula
proofRef ref = do
  i <- except $ note BadRef ref
  { rows, scopes } <- get
  { formula, error } <- except $ note (RefOutOfBounds i) $ rows !! (i - 1)
  when (not (lineInScope i scopes)) $ throwError RefDiscarded
  when (error == Just BadFormula) $ throwError BadRef -- User needs to have input the formula
  except $ note BadRef formula
    >>= case _ of
        FC f -> Right f
        VC _ -> Left $ InvalidArg ArgNotFormula

boxRef :: Maybe Box -> ExceptT NdError ND (Tuple FFC FFC)
boxRef ref = do
  box@(Tuple i j) <- except $ note BadRef_Box ref
  { rows, scopes } <- get
  { formula: f1, error: e1 } <- except $ note (RefOutOfBounds i) $ rows !! (i - 1)
  { formula: f2, error: e2 } <- except $ note (RefOutOfBounds j) $ rows !! (j - 1)
  when (isNotBox box scopes) $ throwError NotABox
  when (not (boxInScope box scopes)) $ throwError RefDiscarded
  when (e1 == Just BadFormula || e2 == Just BadFormula) $ throwError BadRef -- User needs to have input the formula
  except $ note BadRef $ Tuple <$> f1 <*> f2

-- | Attempt to apply the specified rule given the user-provided formula.
-- |
-- | Does not modify state.
-- |
-- | The formula inputted by the user is needed to uniformly handle
-- | rules such as LEM, which violate the subformula property. They
-- | can then return that formula as the result, provided it is valid.
applyRule :: Rule -> Maybe FFC -> ExceptT NdError ND FFC
applyRule rule formula = do
  { rows, scopes } <- get
  -- | Throws an error if "v" shadows any preexistent variables.
  let
    checkShadowing v =
      maybe (pure unit)
        (throwError <<< FreshShadowsVar v)
        $ varIntroRow v scopes
  case rule of
    Premise -> do
      case formula of
        Just (FC _) ->
          if all isPremise rows then
            except $ note BadFormula formula
          else
            throwError BadPremise
        _ -> throwError $ FormulaMismatch PremiseM
    -- TODO Check if this assumption is the first formula in the current box
    Assumption -> except $ note BadFormula formula
    AndElim1 i -> do
      a <- proofRef i
      case a of
        And x _ -> pure $ FC x
        _ -> throwError $ InvalidArg BadAndE
    AndElim2 i -> do
      a <- proofRef i
      case a of
        And _ x -> pure $ FC x
        _ -> throwError $ InvalidArg BadAndE
    AndIntro i j -> FC <$> (And <$> proofRef i <*> proofRef j)
    OrElim i box1 box2 -> do
      a <- proofRef i
      Tuple b1 b2 <- boxRef box1
      Tuple c1 c2 <- boxRef box2
      case a, b1, c1, b2, c2 of
        Or f1 f2, FC b1', FC c1', FC b2', FC c2' ->
          if f1 `equivalent` b1' && f2 `equivalent` c1' && b2' `equivalent` c2' then
            pure b2
          else
            throwError $ InvalidArg BadOrE1
        _, _, _, _, _ -> throwError $ InvalidArg BadOrE2
    OrIntro1 i -> do
      case formula of
        Just f@(FC (Or f1 _)) -> do
          a <- proofRef i
          if a `equivalent` f1 then pure f else throwError $ FormulaMismatch BadOrI_Order
        _ -> throwError $ FormulaMismatch BadOrI_Formula
    OrIntro2 i -> do
      case formula of
        Just f@(FC (Or _ f2)) -> do
          a <- proofRef i
          if a `equivalent` f2 then pure f else throwError $ FormulaMismatch BadOrI_Order
        _ -> throwError $ FormulaMismatch BadOrI_Formula
    ImplElim i j -> do
      a <- proofRef i
      b <- proofRef j
      case a, b of
        Implies x y, z
          | x `equivalent` z -> pure $ FC y
        z, Implies x y
          | x `equivalent` z -> pure $ FC y
        _, _ -> throwError $ InvalidArg BadImplE
    ImplIntro box -> do
      Tuple a b <- boxRef box
      case a, b of
        FC f1, FC f2 -> pure $ FC $ Implies f1 f2
        _, _ -> throwError $ InvalidArg ArgNotFormula
    NegElim i j -> do
      a <- proofRef i
      b <- proofRef j
      if a `equivalent` Not b || Not a `equivalent` b then
        pure (FC bottomProp)
      else
        throwError $ InvalidArg BadNegE
    NegIntro box -> do
      Tuple a b <- boxRef box
      case a, b of
        FC f1, FC f2 -> if f2 == bottomProp then pure $ FC (Not f1) else throwError $ InvalidArg BadNegI
        _, _ -> throwError $ InvalidArg ArgNotFormula
    BottomElim i -> do
      a <- proofRef i
      if a == bottomProp then except $ note BadFormula formula else throwError $ InvalidArg BadBottomE
    DoubleNegElim i -> do
      a <- proofRef i
      case a of
        Not (Not x) -> pure $ FC x
        _ -> throwError $ InvalidArg BadDoubleNegE
    ModusTollens i j -> do
      a <- proofRef i
      b <- proofRef j
      case a, b of
        Implies x y, Not z -> if y `equivalent` z then pure (FC $ Not x) else throwError $ InvalidArg BadMt1
        Not z, Implies x y -> if y `equivalent` z then pure (FC $ Not x) else throwError $ InvalidArg BadMt1
        _, _ -> throwError $ InvalidArg BadMt2
    DoubleNegIntro i -> (FC <<< Not <<< Not) <$> proofRef i
    PBC box -> do
      Tuple a b <- boxRef box
      case a, b of
        FC (Not f), FC f2 -> if f2 == bottomProp then pure $ FC f else throwError $ InvalidArg BadPBC
        _, _ -> throwError $ InvalidArg BadPBC
    LEM -> case formula of
      Just f@(FC (Or f1 f2))
        | f1 `equivalent` Not f2 || f2 `equivalent` Not f1 -> pure f
      _ -> throwError $ FormulaMismatch BadLem
    Copy i -> FC <$> proofRef i
    Fresh -> case formula of
      Just formula'@(VC v) -> checkShadowing v $> formula'
      _ -> throwError $ FormulaMismatch FreshM
    ForallElim i -> do
      f <- proofRef i
      case f, formula of
        Forall v f', Just formula'@(FC fTarget)
          | isJust $ hasSingleSubOf v f' fTarget -> pure formula'
        Forall _ _, _ -> throwError $ FormulaMismatch UnexplainedError
        _, _ -> throwError BadRule
    ForallIntro box -> do
      Tuple a b <- boxRef box
      case formula, a, b of
        _, FC _, _ -> throwError NotAFresh
        _, _, VC _ -> throwError $ InvalidArg ArgNotFormula
        Just formula'@(FC target@(Forall x f)), VC x0, FC g
          -- x0 can not occur anywhere outside its box
          | x0 `Set.member` freeVarsIn target -> throwError (OccursOutsideBox x0)
          | maybe false (_ == Var x0) $ hasSingleSubOf x f g -> pure formula'
        _, VC x0, FC g -> pure $ FC (Forall x0 g)
    ExistsElim i box -> do
      a <- proofRef i
      Tuple b1 b2 <- boxRef box
      case a, b1, b2 of
        Exists x f, FC g, FC χ -> case hasSingleSubOf x f g of
          Just (Var x0)
            | x0 `Set.member` freeVarsIn χ -> throwError (OccursOutsideBox x0)
            | otherwise ->
              checkShadowing x0
                *> case formula of
                    Just (FC input)
                      | input `equivalent` χ -> pure $ FC input
                    _ -> pure $ FC χ
          _ -> throwError BadRule
        _, _, _ -> throwError BadRule
    ExistsIntro i -> do
      a <- proofRef i
      inputFormula <- except $ note BadFormula formula
      case inputFormula of
        FC (Exists v f)
          | isJust $ hasSingleSubOf v f a -> pure inputFormula
        _ -> throwError $ FormulaMismatch UnexplainedError
    EqElim i j -> do
      a <- proofRef i
      b <- proofRef j
      case a, b, formula of
        Predicate "=" [ t1, t2 ], f, Just formula'@(FC g)
          | almostEqual t1 t2 f g -> pure formula'
          | otherwise -> throwError $ FormulaMismatch UnexplainedError
        Predicate "=" [ _, _ ], _, Nothing -> throwError BadFormula
        badEq, _, _ -> throwError $ InvalidArg (BadEq badEq)
    EqIntro -> case formula of
      Just formula'@(FC (Predicate "=" [ t1, t2 ]))
        | t1 == t2 -> pure formula'
      _ -> throwError $ FormulaMismatch UnexplainedError

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
  let
    formula = inputFormula <|> hush result

    error = case inputFormula, result of
      _, Left e -> Just e
      Nothing, Right _ -> Just BadFormula
      Just f, Right g
        | f == g -> Nothing
        | otherwise -> Just $ FormulaMismatch (GenericMismatch (genericMismatchText result))
  modify_ \proof ->
    proof
      { rows = Array.snoc proof.rows { formula, rule, error }
      , scopes = addLineToInnermost (Array.length proof.rows + 1) inputFormula proof.scopes
      }
  where
  genericMismatchText r = case r of
    Right (FC r') -> "Expected: \"" <> show r' <> "\""
    _ -> "Error"

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

-- | Returns, if the specified var is in scope, the first row idx where it is introduced.
varIntroRow :: Variable -> List Scope -> Maybe Int
varIntroRow v = findMap (\s -> v `Map.lookup` s.vars)

-- | Add a box to a scope.
addBox :: Box -> Scope -> Scope
addBox b s = s { boxes = b `Array.cons` s.boxes }

-- | Add a line to a scope.
addLine :: Int -> (Maybe FFC) -> Scope -> Scope
addLine l formula s =
  s
    { lines = l `Set.insert` s.lines
    , vars =
      s.vars
        <|> ( const l
              <$> Set.toMap
                  ( case formula of
                      Just (VC v) -> Set.singleton v
                      Just (FC f) -> freeVarsIn f
                      Nothing -> Set.empty
                  )
          )
    }

-- | Add a box to the innermost scope in a stack of scopes.
addBoxToInnermost :: Box -> List Scope -> List Scope
addBoxToInnermost b (innermost : outerScopes) = addBox b innermost : outerScopes

addBoxToInnermost _ Nil = unsafeCrashWith "Cannot add to an empty list of scopes."

-- | Add a line to the innermost scope in a stack of scopes.
addLineToInnermost :: Int -> (Maybe FFC) -> List Scope -> List Scope
addLineToInnermost l formula (innermost : outerScopes) = addLine l formula innermost : outerScopes

addLineToInnermost _ _ Nil = unsafeCrashWith "Cannot add to an empty list of scopes."
