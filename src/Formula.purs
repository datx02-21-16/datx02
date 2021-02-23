module Formula where

import Prelude
import Data.String.Common (joinWith)
import Data.Foldable (or)

newtype Variable = Variable String

derive newtype instance eqVariable :: Eq Variable
derive newtype instance showVariable :: Show Variable

data Term
  = Var Variable
    -- Function application
  | App String (Array Term)

derive instance eqTerm :: Eq Term

instance showTerm :: Show Term where
  show (Var v) = show v
  show (App f args) = (show f) <> "(" <> (show args) <> ")"

data Formula
  = Predicate String (Array Term)
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula
  | Forall Variable Formula
  | Exists Variable Formula
  | Bottom 

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
  show (Bottom)       = "⊥"

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
  Bottom -> Bottom

containsTerm :: Formula -> Term -> Boolean
containsTerm f t = case f of
  Predicate n args -> or $ map (\t' -> t == t') args
  Not f'           -> containsTerm f' t
  And f1 f2        -> containsTerm f1 t || containsTerm f2 t
  Or f1 f2         -> containsTerm f1 t || containsTerm f2 t
  Implies f1 f2    -> containsTerm f1 t || containsTerm f2 t
  Forall x f'      -> containsTerm f' t
  Exists x f'      -> containsTerm f' t
  Bottom           -> false 