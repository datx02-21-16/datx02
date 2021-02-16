module Formula where

import Prelude
import Data.String.Common (joinWith)

newtype Variable = Variable String

derive newtype instance eqVariable :: Eq Variable
derive newtype instance showVariable :: Show Variable

-- | A term which denotes some object.
-- |
-- | `Var v` represents a variable symbol and `App f args` is function
-- | application, where `f` is a function symbol or a constant if `f`
-- | is nullary.
data Term
  = Var Variable
  | App String (Array Term)

derive instance eqTerm :: Eq Term

instance showTerm :: Show Term where
  show (Var v) = show v
  show (App p []) = p
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

instance showFormula :: Show Formula where
  show (Predicate p []) = p
  show (Predicate p args) = p <> "(" <> (joinWith ", " (show <$> args)) <> ")"
  show (Not phi) = "¬" <> (show phi)
  show (And a b) = "(" <> (show a) <> " ∧ " <> (show b) <> ")"
  show (Or a b) = "(" <> (show a) <> " ∨ " <> (show b) <> ")"
  show (Implies a b) = "(" <> (show a) <> " → " <> (show b) <> ")"
  show (Forall x phi) = "∀" <> (show x) <> " " <> (show phi)
  show (Exists x phi) = "∃" <> (show x) <> " " <> (show phi)

-- | The formula obtained by replacing each free occurrence of x in f by t.
substitution :: Formula -> Term -> Variable -> Formula
substitution f t x = case f of
  Predicate p xs -> let
    replaceTerm (Var v) = if v == x then t else (Var v)
    replaceTerm (App h args) = App h (replaceTerm <$> args)
    in Predicate p (replaceTerm <$> xs)
  Not h -> Not $ substitution h t x
  And a b -> And (substitution a t x) (substitution b t x)
  Or a b -> And (substitution a t x) (substitution b t x)
  Implies a b -> And (substitution a t x) (substitution b t x)
  Forall y _ | x == y -> f
  Forall y phi -> Forall y (substitution phi t x)
  Exists y _ | x == y -> f
  Exists y phi -> Exists y (substitution phi t x)
