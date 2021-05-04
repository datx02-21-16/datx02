module Formula
  ( Variable(..)
  , Term(..)
  , Formula(..)
  , FFC(..)
  , bottomProp
  , Substitution
  , singleSub
  , class Substitutable
  , substitute
  , disagreementSet
  , unify
  , containsTerm
  , formulaUnifier
  , equalityProp
  , isPropFormula
  , isUnifierVar
  , almostEqual
  , allVarsInFormula
  ) where

import Prelude
import Data.Array (zipWith)
import Data.Array as Array
import Data.Foldable (and, any, find, foldl, or)
import Data.List (List(Nil), null, transpose)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Common (joinWith)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable as Unfoldable

-- | A variable symbol.
newtype Variable
  = Variable String

derive newtype instance eqVariable :: Eq Variable

derive newtype instance ordVariable :: Ord Variable

derive newtype instance semigroupVariable :: Semigroup Variable

instance showVariable :: Show Variable where
  show (Variable s) = s

-- | A term which denotes some object.
-- |
-- | `Var v` represents a variable symbol and `App f args` is function
-- | application, where `f` is a function symbol or a constant if `f`
-- | is nullary.
data Term
  = Var Variable
  | App String (Array Term)

derive instance eqTerm :: Eq Term

derive instance ordTerm :: Ord Term

instance showTerm :: Show Term where
  show (Var v) = show v
  show (App p args) = p <> "(" <> (joinWith ", " (show <$> args)) <> ")"

-- | A formula in first-order logic.
data Formula
  = Predicate String (Array Term)
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula
  | Forall Variable Formula
  | Exists Variable Formula

derive instance eqFormula :: Eq Formula

derive instance ordFormula :: Ord Formula

-- | Shows formulas using least amount of parentheses wrt. precedence.
instance showFormula :: Show Formula where
  show = showPrec 1
    where
    parens s = "(" <> s <> ")"

    optParens b s = if b then parens s else s

    showPrec _ (Predicate p []) = p

    showPrec _ (Predicate p args) = p <> parens (joinWith ", " (show <$> args))

    showPrec _ (Not f) = "¬" <> showPrec 4 f

    showPrec _ (Forall x f) = "∀" <> show x <> " " <> showPrec 4 f

    showPrec _ (Exists x f) = "∃" <> show x <> " " <> showPrec 4 f

    showPrec n (And a b) =
      optParens (n > 3)
        $ showPrec 3 a
        <> " ∧ "
        <> showPrec 4 b

    showPrec n (Or a b) =
      optParens (n > 2)
        $ showPrec 2 a
        <> " ∨ "
        <> showPrec 3 b

    showPrec n (Implies a b) =
      optParens (n > 1)
        $ showPrec 2 a
        <> " → "
        <> showPrec 1 b

data FFC
  = FC Formula
  | VC Variable

derive instance eqFFC :: Eq FFC

instance showFFC :: Show FFC where
  show (FC f) = "FC: " <> show f
  show (VC v) = "VC: " <> show v

-- | Dedicated symbol for a proposition that is assigned a false truth value.
bottomProp :: Formula
bottomProp = Predicate "⊥" []

equalityProp :: Term -> Term -> Formula
equalityProp t1 t2 = Predicate "=" [ t1, t2 ]

-- | A substitution {t₁/v₁, ..., tₙ/vₙ}.
-- |
-- | All v:s must be mutually unique.
data Substitution
  = Substitution (Map Variable Term)

derive instance eqSubstitution :: Eq Substitution

instance showSubstitution :: Show Substitution where
  show (Substitution ss) =
    let
      elems = Map.toUnfoldableUnordered ss :: Array _

      showElem (Tuple t v) = show t <> "/" <> show v
    in
      "{" <> (joinWith ", " $ showElem <$> elems) <> "}"

instance semigroupSubstitution :: Semigroup Substitution where
  -- | The composition of the two substitutions.
  append (Substitution a) λ@(Substitution b) =
    Substitution
      $ Map.mapMaybeWithKey
          ( \x t ->
              let
                t' = substitute λ t
              in
                if t' == Var x then Nothing else Just t'
          )
          a
          `Map.union`
            b

instance monoidSubstitution :: Monoid Substitution where
  mempty = Substitution Map.empty

-- | Returns a singleton substitution.
-- |
-- | The variable v may not occur in the replacement term t.
singleSub :: Variable -> Term -> Maybe Substitution
singleSub v t =
  if v `occursIn` t then
    Nothing
  else
    Just $ Substitution (Map.singleton v t)
  where
  occursIn v1 = case _ of
    Var v2 -> v1 == v2
    App _ args -> any (v1 `occursIn` _) args

-- | Returns a singleton substition {y/x}.
varSub :: Variable -> Variable -> Substitution
varSub x y = Substitution $ Map.singleton x (Var y)

-- | Expressions on which substitutions can be done.
class Substitutable a where
  -- | Performs the specified substitution on the given expression (eθ).
  -- |
  -- | Returns the new expression obtained by simultaneously replacing
  -- | each occurence of the free substituted variables by their
  -- | respective replacements.
  substitute :: Substitution -> a -> a

instance substitutableTerm :: Substitutable Term where
  substitute θ@(Substitution s) = case _ of
    Var v1 -> fromMaybe (Var v1) $ Map.lookup v1 s
    App f args -> App f $ (substitute θ) <$> args

instance substitutableFormula :: Substitutable Formula where
  substitute θ@(Substitution s) f = case f of
    Predicate p args -> Predicate p (substitute θ <$> args)
    Not a -> Not $ sub a
    And a b -> And (sub a) (sub b)
    Or a b -> And (sub a) (sub b)
    Implies a b -> Implies (sub a) (sub b)
    Forall x a -> Forall x (subWithout x a)
    Exists x a -> Exists x (subWithout x a)
    where
    sub = substitute θ

    subWithout x = substitute $ Substitution (Map.delete x s)

-- | The disagreement set of the specified set of expression lists.
-- |
-- | It is the set of respective subexpressions where the expressions
-- | first differ when doing a depth-first traversal.
disagreementSet :: List (List Term) -> List Term
disagreementSet xs = fromMaybe Nil $ List.find (not <<< null) $ disagreement <$> transpose xs
  where
  -- | Returns subexpressions of a term if it shares top-most structure with head.
  subexprs head = case head, _ of
    Var v1, Var v2
      | v1 == v2 -> Just Nil
    App f1 args1, App f2 args2
      | f1 == f2, Array.length args1 == Array.length args2 -> Just $ List.fromFoldable args2
    _, _ -> Nothing

  disagreement terms = case List.head terms of
    Nothing -> Nil
    Just head -> maybe terms disagreementSet (sequence $ subexprs head <$> terms)

-- | Tries to find a most general unifier for the set of term lists.
-- |
-- | Implements the unification algorithm.
-- |
-- | See: C. Chang and R. Lee "Symbolic Logic and Mechanical Theorem
-- |      Proving". Academic Press, New York, 1973
unify :: Set (List Term) -> Maybe (Tuple Substitution (List Term))
unify = go mempty
  where
  -- If w is singleton σ is most general unifier
  go σ w
    | Set.size w <= 1 = (Tuple σ) <$> (Set.toUnfoldable w)

  go σ w =
    let
      ds = disagreementSet $ List.fromFoldable w

      sub =
        List.head do
          -- Find v and t in ds such that v is a variable not occuring in t
          v <-
            ds
              >>= case _ of
                  Var v -> pure v
                  _ -> Nil
          t <- ds
          Unfoldable.fromMaybe $ singleSub v t
    in
      sub >>= (\λ -> go (σ <> λ) (Set.map (map (substitute λ)) w))

containsTerm :: Formula -> Term -> Boolean
containsTerm f t = case f of
  Predicate _ args -> or $ map (\t' -> t == t') args
  Not f' -> containsTerm f' t
  And f1 f2 -> containsTerm f1 t || containsTerm f2 t
  Or f1 f2 -> containsTerm f1 t || containsTerm f2 t
  Implies f1 f2 -> containsTerm f1 t || containsTerm f2 t
  Forall x f' -> t /= Var x && containsTerm f' t
  Exists x f' -> t /= Var x && containsTerm f' t

-- | Returns a new variable that does not conflict with any
-- | pre-existent variable in the specified formula.
-- |
-- | Guaranteed to be the concatenation of all vars in the formula.
varUniqueIn :: Formula -> Variable
varUniqueIn = Variable <<< foldl (<>) "" <<< allVarsInFormula

allVarsInFormula :: Formula -> Array String
allVarsInFormula = case _ of
  Predicate _ args -> args >>= allVarsInTerm
  Not f -> allVarsInFormula f
  And f1 f2 -> allVarsInFormula f1 <> allVarsInFormula f2
  Or f1 f2 -> allVarsInFormula f1 <> allVarsInFormula f2
  Implies f1 f2 -> allVarsInFormula f1 <> allVarsInFormula f2
  Forall (Variable x) f -> [ x ] <> allVarsInFormula f
  Exists (Variable x) f -> [ x ] <> allVarsInFormula f
  where
  allVarsInTerm = case _ of
    Var (Variable s) -> [ s ]
    App _ args -> args >>= allVarsInTerm

-- | Unify the terms in the two formulas.
formulaUnify :: Formula -> Formula -> Maybe (Tuple Substitution (List Term))
formulaUnify f1 f2 = subTerms f1 f2 >>= unify
  where
  -- | Returns the subterms if the two formulas have the same structure.
  subTerms :: Formula -> Formula -> Maybe (Set (List Term))
  subTerms = case _, _ of
    Predicate f args1, Predicate g args2
      | f == g, Array.length args1 == Array.length args2 ->
        Just
          $ Set.fromFoldable
              [ List.fromFoldable args1, List.fromFoldable args2 ]
    Not a, Not b -> subTerms a b
    And a b, And c d -> (<>) <$> subTerms a c <*> subTerms b d
    Or a b, Or c d -> (<>) <$> subTerms a c <*> subTerms b d
    Implies a b, Implies c d -> (<>) <$> subTerms a c <*> subTerms b d
    Forall x f, Forall y g -> subTermsQuantified x f y g
    Exists x f, Exists y g -> subTermsQuantified x f y g
    _, _ -> Nothing

  subTermsQuantified :: Variable -> Formula -> Variable -> Formula -> Maybe (Set (List Term))
  subTermsQuantified x f y g =
    let
      gWithoutX = substitute (varSub x (y <> varUniqueIn f <> varUniqueIn g)) g

      gWithXInsteadOfY = if x == y then g else substitute (varSub y x) gWithoutX
    in
      case formulaUnify f gWithXInsteadOfY of
        Nothing -> Nothing
        -- Disallow unification of ∃x P(x) and ∃y P(z)
        Just (Tuple (Substitution s) _)
          | x `Map.member` s
              || (isJust $ find (_ == Var x) $ Map.values s) -> Nothing
        Just _ -> subTerms f gWithXInsteadOfY

formulaUnifier :: Formula -> Formula -> Maybe Substitution
formulaUnifier a b = (\(Tuple σ _) -> σ) <$> formulaUnify a b

isUnifierVar :: Variable -> Formula -> Formula -> Boolean
isUnifierVar v f1 f2 =
  v `isIn` f1
    && not (v `isIn` f2)
    && maybe false (\(Substitution s) -> Map.size s == 1) (formulaUnifier f1 f2)
  where
  isIn (Variable v) f = v `Array.elem` allVarsInFormula f

almostEqual :: Term -> Term -> Formula -> Formula -> Boolean
almostEqual t1 t2 = go Set.empty
  where
  go boundVars f1 f2 = case f1, f2 of
    Predicate f args1, Predicate g args2
      | f == g && Array.length args1 == Array.length args2 -> and $ zipWith equalsOrSub args1 args2
    Not a, Not b -> go boundVars a b
    And a b, And c d -> go boundVars a c && go boundVars b d
    Or a b, Or c d -> go boundVars a c && go boundVars b d
    Implies a b, Implies c d -> go boundVars a c && go boundVars b d
    Forall x f, Forall y g -> go (Set.insert (Var y) $ Set.insert (Var x) boundVars) f g
    Exists x f, Exists y g -> go (Set.insert (Var y) $ Set.insert (Var x) boundVars) f g
    _, _ -> false
    where
    equalsOrSub a1 a2 =
      a1 == a2
        || (not (a1 `Set.member` boundVars || a2 `Set.member` boundVars) && a1 == t1 && a2 == t2)

isPropFormula :: Formula -> Boolean
isPropFormula formula = case formula of
  Predicate _ [] -> true
  Not f' -> isPropFormula f'
  And f' f'' -> isPropFormula f' && isPropFormula f''
  Or f' f'' -> isPropFormula f' && isPropFormula f''
  Implies f' f'' -> isPropFormula f' && isPropFormula f''
  _ -> false
