-- | This module implements the algorithm for automated first-order
-- | natural deduction from Bolotov, A., Bocharov, V., Gorchakov, A.,
-- | & Shangin, V. (2005). Automated first order natural deduction.
-- | ISCAI.

module NdAlg (Rule(..), prove) where

import Prelude
import Data.Array as Array
import Data.Array ((..), (!!), snoc, unsafeIndex)
import Data.Maybe (Maybe(..), maybe', isJust, fromJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Foldable (any, findMap)
import Util (enumerate, traceShowId)
import Data.List (List(Nil), (:))
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Control.Alternative ((<|>))
import Control.MonadZero (guard)
import Data.Lazy (defer, force)

import Formula (Variable(..), Term(..), Formula(..), formulaUnifier)

data Rule
  = Premise
  | Assumption
  | AndElim Int
  | AndIntro
  | OrElim Int Int
  | OrIntro
  | NotElim Int
  | NotIntro
  | ImpliesElim Int Int
  | ImpliesIntro

derive instance eqRule :: Eq Rule
instance showRule :: Show Rule where
  show Premise = "Premise"
  show Assumption = "Assumption"
  show (AndElim i) = "∧e, " <> show i
  show (AndIntro) = "∧i"
  show (OrElim i j) = "∨e, " <> show i <> "," <> show j
  show (OrIntro) = "∨i"
  show (NotElim i) = "¬e, " <> show i
  show (NotIntro) = "¬i"
  show (ImpliesElim i j) = "→e, " <> show i <> "," <> show j
  show (ImpliesIntro) = "→i"

-- | A derived formula.
type Derived = { formula :: Formula, rule :: Rule }

data Goal = False | Goal Formula

derive instance eqGoal :: Eq Goal
instance showGoal :: Show Goal where
  show False = "false"
  show (Goal formula) = show formula

-- | A partial or completed algorithmic natural deduction derivation.
type NdAlg = { list_proof :: Array Derived
             , list_goals :: List Goal
             , discarded :: Set Int
             , markedElim :: Set Int
             , markedComplex :: Set Formula
             }

nonDiscardedProofsEnum :: NdAlg -> Array { i :: Int, x :: Derived }
nonDiscardedProofsEnum { list_proof, discarded }
  = Array.filter notDiscarded $ enumerate list_proof
  where
    notDiscarded { i } = not $ i `Set.member` discarded

nonDiscardedProofs :: NdAlg -> Array Formula
nonDiscardedProofs nd = _.x.formula <$> nonDiscardedProofsEnum nd

goalReached :: Goal -> NdAlg -> Boolean
goalReached goal nd = case goal of
  Goal g -> any (isJust <<< formulaUnifier g) proofs
  False -> not $ Array.null do
    b <- proofs >>= case _ of Not b -> pure b
                              _ -> []
    a <- proofs
    guard $ isJust (formulaUnifier a b)
  where proofs = nonDiscardedProofs nd

findElim :: NdAlg -> Maybe NdAlg
findElim nd@{ list_proof, list_goals, markedElim }
  = (\{newProofs, fromProof}
     -> nd { list_proof = list_proof <> newProofs
           , markedElim = Set.insert fromProof markedElim })
    <$> findMap considerFormula proofs
  where
    proofs = nonDiscardedProofsEnum nd

    considerFormula { i, x: {formula: f1} }
      =
        -- If not already considered for elimination
        if i `Set.member` markedElim then Nothing
        else
          { newProofs: _, fromProof: i } <$> (unaryElim <|> binaryElim)
        where
          unaryElim :: Maybe (Array Derived)
          unaryElim = case f1 of
            And a b -> Just [ { formula: a, rule: AndElim i }
                            , { formula: b, rule: AndElim i } ]
            Not (Not a) -> Just [{ formula: a, rule: NotElim i }]
            _ -> Nothing

          binaryElim = findMap
                       (\{i: i2, x: {formula: f2}} -> binaryElimWithFormulas i2 f2)
                       proofs

          binaryElimWithFormulas :: Int -> Formula -> Maybe (Array Derived)
          binaryElimWithFormulas i2 = case f1, _ of
            Or a b, Not c | a == c -> Just [{ formula: b, rule: OrElim i i2 }]
            Implies a b, c | a == c -> Just [{ formula: b, rule: ImpliesElim i i2 }]
            _, _ -> Nothing
    -- TODO: forall/exists elim

doIntro :: NdAlg -> NdAlg
doIntro nd@{ list_proof
           , list_goals
           , discarded
           , markedElim
           , markedComplex }
  = case list_goals of
  -- If successfully proved the first conjunct: Now prove the second
  Goal _:next@(Goal _:Goal (And _ _):_) -> nd { list_goals = next }

  -- Only way we could have reached this goal is by first proving `a` and `b`
  _:Goal and@(And a b):next
    -> nd { list_proof = snoc list_proof { formula: and, rule: AndIntro }
          , list_goals = next}

  -- Note: →i/¬i will free up any markedElim/Complex whose result gets discarded
  -- (Can be made less messy by storing in boxes, which are introduced on adding an assumption)

  _:Goal implies@(Implies a b):next -- do implies intro (and discard)
    -> nd { list_proof = snoc list_proof { formula: implies, rule: ImpliesIntro }
          , list_goals = next
          , discarded = discarded `Set.union` unsafePartial (fromJust untilMostRecentAssumption)
          -- , markedElim = markedElim `Set.difference` unsafePartial (fromJust untilMostRecentAssumption)
          , markedComplex = markedComplex `Set.difference` unsafePartial (fromJust discardedProofs) }

  -- Proof by contradiction
  False:next@(Goal a:_)
    -> nd { list_proof = snoc list_proof { formula: Not (unsafePartial $ fromJust mostRecentAssumption), rule: NotIntro }
          , list_goals = next
          , discarded = discarded `Set.union` unsafePartial (fromJust untilMostRecentAssumption)
                 -- , markedElim = markedElim `Set.difference` unsafePartial (fromJust untilMostRecentAssumption)
          , markedComplex = markedComplex `Set.difference` unsafePartial (fromJust discardedProofs) }

  _:Goal or@(Or a b):next
    -> nd { list_proof = snoc list_proof { formula: or, rule: OrIntro }
          , list_goals = next}

  -- No applicable introduction rules: Just pop it
  _:next -> nd { list_goals = next }

  _ -> unsafeCrashWith $ "not yet implemented: " <> show nd
  where
    proofs = nonDiscardedProofsEnum nd

    lastAssumption = 0

    mostRecentAssumption = _.x.formula <$> (Array.findLastIndex ((_ == Assumption) <<< _.x.rule) proofs
                           >>= (proofs !! _))

    discardedProofs
      = (\is -> Set.mapMaybe ((_.formula <$> _) <<< (list_proof !! _)) is)
        <$> untilMostRecentAssumption

    untilMostRecentAssumption :: Maybe (Set Int)
    untilMostRecentAssumption
      = (\i -> let {i: assIdx} = unsafePartial $ unsafeIndex proofs i
          in Set.fromFoldable (assIdx..(Array.length list_proof - 1)))
        <$> Array.findLastIndex (\{x: {rule}} -> rule == Assumption) proofs

-- | Tries to prove the specified formula under the given antecedents.
prove :: Array Formula -> Formula -> Maybe (Array Derived)
prove premises initialGoal
  = _.list_proof <$> (go $ { list_proof: { formula: _, rule: Premise } <$> premises
                           , list_goals: Goal initialGoal : Nil
                           , discarded: Set.empty
                           , markedElim: Set.empty
                           , markedComplex: Set.empty })
  where go nd@{ list_goals: Nil } = Just nd -- No goals means we are finished

        go nd@{ list_goals: goal:_ } | goalReached goal nd = go $ doIntro nd

        go nd@{ list_goals: goal:_ }
          = maybe' (\_ -> branchOnCurrentGoal nd goal) go $ findElim nd

        -- Procedure 2
        branchOnCurrentGoal nd@{ list_proof, list_goals, markedComplex }
          = case _ of
          -- Procedure 2.1
          -- For elementary quantifier free formula (with negated prop handled below)
          Goal p@(Predicate _ _)
            -> go $ nd { list_proof = snoc list_proof {formula: Not p, rule: Assumption}
                       , list_goals = False:list_goals }
          -- ¬A, where A can be a literal, disjunction or exists quantified formula
          Goal (Not a) | literalDisjunctionOrExists a
            -> go $ nd { list_proof = snoc list_proof {formula: a, rule: Assumption}
                       , list_goals = False:list_goals }

          Goal (And a b) -> go $ nd { list_goals = Goal b:Goal a:list_goals }
          -- Try either disjunct
          Goal (Or a b) -> findMap force
                             [ defer \_ -> go $ nd { list_goals = Goal a:list_goals }
                             , defer \_ -> go $ nd { list_goals = Goal b:list_goals }
                             , defer \_ -> go $ nd { list_proof = snoc list_proof {formula: Not (Or a b), rule: Assumption}
                                                   , list_goals = False:list_goals } ]

          Goal (Implies a b)
            -> go $ nd { list_proof = snoc list_proof {formula: a, rule: Assumption }
                       , list_goals = Goal b:list_goals }

          Goal (Forall α a) -> unsafeCrashWith "not yet implemented"
          Goal (Exists α a) -> unsafeCrashWith "not yet implemented"

          -- Procedure 2.2
          False -> let
            goalFromStructure = case _ of
              Not a -> Just a
              Or a _ -> Just $ Not a
              Implies a _ -> Just a
              _ -> Nothing
            newGoals
              = Array.mapMaybe (\{i, x: {formula: f}} -> goalFromStructure f
                                                         >>= \g -> if g `Set.member` markedComplex
                                                                   then Nothing
                                                                   else pure {i, goal: g})
                $ nonDiscardedProofsEnum nd
            tryReachGoal {i, goal} = go $ nd { list_goals = Goal goal:list_goals
                                             , markedComplex = Set.insert goal markedComplex }
            in findMap tryReachGoal newGoals

          Goal (Not a)
            -> go $ nd { list_proof = snoc list_proof {formula: a, rule: Assumption}
                       , list_goals = False:list_goals }

        literalDisjunctionOrExists = case _ of
          Predicate _ [] -> true
          Not (Predicate _ []) -> true
          Or _ _ -> true
          Exists _ _ -> true
          _ -> false
