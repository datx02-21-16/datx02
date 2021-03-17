-- | This module implements the algorithm for automated first-order
-- | natural deduction from Bolotov, A., Bocharov, V., Gorchakov, A.,
-- | & Shangin, V. (2005). Automated first order natural deduction.
-- | ISCAI.

module NdAlg (Rule(..), prove) where

import Prelude
import Data.Array as Array
import Data.Array (snoc, slice)
import Data.Maybe (Maybe(..), maybe', isJust, fromJust)
import Data.Set (Set)
import Data.Set as Set
import Data.FunctorWithIndex (mapWithIndex)
import Data.Foldable (any, findMap)
import Data.Traversable (mapAccumL)
import Data.List as List
import Data.List (List(Nil), (:))
import Data.NonEmpty as NonEmpty
import Data.NonEmpty (NonEmpty, (:|))
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Control.Alternative ((<|>))
import Control.MonadZero (guard)

import Formula (Variable(..), Term(..), Formula(..), formulaUnifier)
import Util (findLast)

-- | The minimal set of elimination and introduction rules.
data Rule
  = Premise
  | Assumption
  | AndElim Int
  | AndIntro
  | OrElim Int Int
  | OrIntro
  | NotElim Int
  | NotIntro Int
  | ImpliesElim Int Int
  | ImpliesIntro Int

derive instance eqRule :: Eq Rule
instance showRule :: Show Rule where
  show Premise = "Premise"
  show Assumption = "Assumption"
  show (AndElim i) = "∧e " <> show i
  show (AndIntro) = "∧i"
  show (OrElim i j) = "∨e " <> show i <> "," <> show j
  show (OrIntro) = "∨i"
  show (NotElim i) = "¬e " <> show i
  show (NotIntro i) = "¬i " <> show i
  show (ImpliesElim i j) = "→e " <> show i <> "," <> show j
  show (ImpliesIntro i) = "→i " <> show i

-- | A goal for the proof searching strategy.
-- |
-- | Either a formula or two arbitrary contradictory formulae.
data Goal = Goal Formula | False

derive instance eqGoal :: Eq Goal

instance showGoal :: Show Goal where
  show False = "false"
  show (Goal formula) = show formula

-- | Inclusive-exclusive interval ordered by its left endpoint.
data Interval = Interval Int Int

instance showInterval :: Show Interval where
  show (Interval a b) = "[" <> show a <> "," <> show b <> ")"

derive instance eqInterval :: Eq Interval

instance ordInterval :: Ord Interval where
  compare (Interval a b) (Interval c d) = a `compare` c

-- | Marking scheme to prevent infinite looping.
type Marks = { elim :: Set Int
             , complex :: Set Formula }

-- | A partial or completed algorithmic natural deduction derivation.
type NdAlg = { list_proof :: Array { formula :: Formula, rule :: Rule }
             , list_goals :: List Goal
             , discards :: Set Interval
             , marks :: NonEmpty List Marks -- One for each open nested box
             }

-- | The set of proofs that are not yet discarded.
nonDiscarded :: NdAlg -> Array { i :: Int, x :: { formula :: Formula, rule :: Rule } }
nonDiscarded { list_proof, discards }
  = Array.concatMap
    (\(Interval a b) -> mapWithIndex (\i x -> { i: i + a, x })
                        $ slice a b list_proof)
    nonDiscardedIntervals
  where
    intervals = Set.toUnfoldable discards -- Note: Result is ordered
    { value: nonDiscardedIntervals }
      = mapAccumL
        (\i (Interval a b) -> { accum: max i b, value: Interval i a })
        0 $ snoc intervals (Interval (Array.length list_proof) 0)

-- | Reachability status of a current goal.
goalReached :: Goal -> NdAlg -> Boolean
goalReached goal nd = case goal of
  Goal g -> any (unifiable g) proofs
  False -> not $ Array.null do
    b <- proofs >>= case _ of Not b -> pure b
                              _ -> []
    a <- proofs
    guard $ unifiable a b
  where
    proofs = _.x.formula <$> nonDiscarded nd
    unifiable a b = isJust $ formulaUnifier a b

findElim :: NdAlg -> Maybe NdAlg
findElim nd@{ list_proof, list_goals, marks: marks@(m:|ms) }
  = (\{newProofs, fromProof}
     -> nd { list_proof = list_proof <> newProofs
           , marks = m { elim = Set.insert fromProof m.elim }:|ms })
    <$> findMap considerFormula proofs
  where
    proofs = nonDiscarded nd

    considerFormula { i, x: {formula: f1} }
      =
        -- If not already considered for elimination
        if any (\{elim} -> i `Set.member` elim) marks then Nothing
        else
          { newProofs: _, fromProof: i } <$> (unaryElim <|> binaryElim)
        where
          unaryElim :: Maybe (Array { formula :: Formula, rule :: Rule })
          unaryElim = case f1 of
            And a b -> Just [ { formula: a, rule: AndElim i }
                            , { formula: b, rule: AndElim i } ]
            Not (Not a) -> Just [{ formula: a, rule: NotElim i }]
            _ -> Nothing

          binaryElim = findMap
                       (\{i: i2, x: {formula: f2}} -> binaryElimWithFormulas i2 f2)
                       proofs

          binaryElimWithFormulas :: Int -> Formula -> Maybe (Array { formula :: Formula, rule :: Rule })
          binaryElimWithFormulas i2 = case f1, _ of
            Or a b, Not c | a == c -> Just [{ formula: b, rule: OrElim i i2 }]
            Implies a b, c | a == c -> Just [{ formula: b, rule: ImpliesElim i i2 }]
            _, _ -> Nothing
    -- TODO: forall/exists elim

doIntro :: NdAlg -> NdAlg
doIntro nd@{ list_proof
           , list_goals
           , discards
           , marks }
  = case list_goals of
  -- Do implies-introduction (and discard up till assumption)
  _:Goal implies@(Implies _ _):next -> let
    ass = unsafePartial $ fromJust lastAssumption
    in nd { list_proof = snoc list_proof { formula: implies, rule: ImpliesIntro $ Array.length list_proof - 1 }
          , list_goals = next
          , discards = Interval ass.i (Array.length list_proof) `Set.insert` discards
          , marks = popMarks marks }

  -- Proof by contradiction
  False:next@(Goal _:_) -> let
    ass = unsafePartial $ fromJust lastAssumption
    in nd { list_proof = snoc list_proof { formula: Not ass.x.formula, rule: NotIntro $ Array.length list_proof - 1 }
          , list_goals = next
          , discards = Interval ass.i (Array.length list_proof) `Set.insert` discards
          , marks = popMarks marks }

  -- Only way we could have reached this goal is by first proving conjuncts
  _:Goal and@(And _ _):next
    -> nd { list_proof = snoc list_proof { formula: and, rule: AndIntro }
          , list_goals = next}

  _:Goal or@(Or _ _):next
    -> nd { list_proof = snoc list_proof { formula: or, rule: OrIntro }
          , list_goals = next}

  -- No applicable introduction rules: Just pop goal
  _:next -> nd { list_goals = next }

  _ -> unsafeCrashWith $ "not yet implemented: " <> show nd
  where
    proofs = nonDiscarded nd
    lastAssumption = findLast ((_ == Assumption) <<< _.x.rule) proofs
    -- Panics if we are not in a nested box
    popMarks :: NonEmpty List Marks -> NonEmpty List Marks
    popMarks ms
      = unsafePartial $ fromJust
        $ (\{head, tail} -> head:|tail) <$> List.uncons (NonEmpty.tail ms)

-- | Tries to prove the specified formula under the given antecedents.
prove :: Array Formula -> Formula -> Maybe (Array { formula :: Formula, rule :: Rule })
prove premises initialGoal
  = _.list_proof <$> (go $ { list_proof: { formula: _, rule: Premise } <$> premises
                           , list_goals: Goal initialGoal:Nil
                           , discards: Set.empty
                           , marks: {elim: Set.empty, complex: Set.empty}:|Nil
                           })
  where go nd@{ list_goals: Nil } = Just nd -- No goal means we are finished
        go nd@{ list_goals: goal:_ } | goalReached goal nd = go $ doIntro nd
        go nd@{ list_goals: goal:_ }
          = maybe' (\_ -> branchOnCurrentGoal nd goal) go $ findElim nd

        literalDisjunctionOrExists = case _ of
          Predicate _ [] -> true
          Not (Predicate _ []) -> true
          Or _ _ -> true
          Exists _ _ -> true
          _ -> false

        -- Procedure 2
        branchOnCurrentGoal nd@{ list_proof, list_goals, marks: marks@(m:|ms) }
          = case _ of
          -- Procedure 2.1
          -- For elementary quantifier free formula (with negation handled below)
          Goal p@(Predicate _ _)
            -> go $ nd { list_proof = snoc list_proof {formula: Not p, rule: Assumption}
                       , list_goals = False:list_goals
                       , marks = m:|m:ms }
          Goal (Not a) --- | literalDisjunctionOrExists a
            -> go $ nd { list_proof = snoc list_proof {formula: a, rule: Assumption}
                       , list_goals = False:list_goals
                       , marks = m:|m:ms }
          Goal (And a b) -> go $ nd { list_goals = Goal a:Goal b:list_goals }
          -- Try either disjunct
          Goal (Or a b) -> findMap go
                             [ nd { list_goals = Goal a:list_goals }
                             , nd { list_goals = Goal b:list_goals }
                             -- Only try to show ¬A ∧ ¬B if necessary
                             , nd { list_proof = snoc list_proof {formula: Not (Or a b), rule: Assumption}
                                  , list_goals = False:list_goals
                                  , marks = m:|m:ms }
                             , nd { list_proof = snoc list_proof {formula: Not (Or a b), rule: Assumption}
                                  , list_goals = Goal (Not a):Goal (Not b):False:list_goals
                                  , marks = m:|m:ms } ]
          Goal (Implies a b)
            -> go $ nd { list_proof = snoc list_proof {formula: a, rule: Assumption }
                       , list_goals = Goal b:list_goals
                       , marks = m:|m:ms }
          Goal (Forall α a) -> unsafeCrashWith "not yet implemented"
          Goal (Exists α a) -> unsafeCrashWith "not yet implemented"

          -- Procedure 2.2
          False -> let
            newGoalFrom = case _ of
              Not a -> Just a
              Or a _ -> Just $ Not a
              Implies a _ -> Just a
              _ -> Nothing
            considerForNewGoal {x: {formula: f}} = do
              guard $ not $ any (\{complex} -> f `Set.member` complex) marks
              goal <- newGoalFrom f
              go $ nd { list_goals = Goal goal:list_goals
                      , marks = m { complex = f `Set.insert` m.complex }:|ms }
            in findMap considerForNewGoal $ nonDiscarded nd
