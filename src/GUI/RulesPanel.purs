module GUI.RulesPanel where

import Prelude (Unit, Void, discard, identity, pure, ($), (<>))
import Type.Proxy (Proxy(..))
import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import GUI.Proof as GP
import GUI.Rules as R

import Data.Maybe
import Data.Array as Array

type Slots = ( proofPanel ::                H.Slot Query Void Int
             , proof      :: forall output. H.Slot GP.Query output Int)

_proofPanel = Proxy :: Proxy "proofPanel"
_proof      = Proxy :: Proxy "proof"

data Query a = Tell Output a

proofPanel :: forall input output m. MonadEffect m => H.Component Query input output m
proofPanel =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval { handleQuery = handleQuery}
    }
  where
  
  handleQuery :: forall a state action. Query a -> H.HalogenM state action Slots output m (Maybe a)
  handleQuery (Tell command a) = do
    H.tell _proof 0 (GP.Tell command)
    pure (Just a)

  render :: forall state action. state -> H.ComponentHTML action Slots m
  render _ =
    HH.div
      [ HP.classes [ HH.ClassName "panel", HH.ClassName "is-primary" ] ]
      [ HH.p
          [ HP.classes [ HH.ClassName "panel-heading" ] ]
          [ HH.text "Proof" ]
      , HH.div
          [ HP.classes [ HH.ClassName "panel-block" ] ]
          [ HH.slot_ _proof 0 GP.proof { } ]
      ]

type Action = R.Rules
type Output = R.Rules

type Hint = { sequent     :: String
            , textualHint :: String
            }

type State = Maybe Hint

ruleButtonPanel :: forall query m . MonadEffect m => H.Component query Int Output m
ruleButtonPanel =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction}
    }
  where
  initialState _ = Nothing

  render st =
    HH.div
      [ HP.classes [ HH.ClassName "panel", HH.ClassName "is-primary" ] ] $
      [ HH.p
          [ HP.classes [ HH.ClassName "panel-heading" ] ]
          [ HH.text "Rules" ]
      ] <>
      Array.mapWithIndex createButton buttonText <>
      [hintBox st]
   
  createButton i text = HH.button
                          [ HP.classes [ HH.ClassName "button"]
                          , HP.type_ HP.ButtonSubmit
                          , HE.onClick $ \_ -> i
                          ]
                          [ HH.text text ]

  hintBox st =
    HH.div
      [HP.classes [ HH.ClassName "box" ]]
      [ HH.div
          [HP.classes [HH.ClassName "centered-text"]]
          [HH.text $ maybe "" (\{sequent: tp} -> tp) st]
      , HH.br_
      , HH.div
          [HP.classes [HH.ClassName "centered-text"]]
          [HH.text $ maybe "" (\{textualHint: th} -> th) st]
      ]
  
  buttonText :: Array String
  buttonText = [ "Premise"
               , "Ass."
               , "∧e1"
               , "∧e2"
               , "∧i"
               , "∨e"
               , "∨i1"
               , "∨i2"
               , "→e"
               , "→i"
               , "¬e"
               , "¬i"
               , "⊥e"
               , "¬¬e"
               , "MT"
               , "¬¬i"
               , "PBC"
               , "LEM"
               , "Copy"
               ]

  ruleHints :: Array Hint
  ruleHints = [ { sequent: "⊢ A"
                , textualHint: "A premise is something that is assumed to be universally true, " <>
                               "and therefore need no justification."}
              , { sequent: "(⊢ A)" -- Not sure if this is correct or how to (or even if we should) represent assumption as a sequent?
                , textualHint: "An assumption introduces a new fact without justification, " <>
                               "but does so inside a new scope. The assumption or any conclusions " <>
                               "drawn from it is not allowed to escape this scope."}
              , { sequent: "A ∧ B ⊢ A"
                , textualHint: "The left conjunction elimination rule concludes " <>
                               "the formula A from the premise A∧B."}
              , { sequent: "A ∧ B ⊢ B"
                , textualHint: "The right conjunction elimination rule concludes " <>
                               "the formula B from the premise A∧B."}
              , { sequent: "A,B ⊢ A ∧ B"
                , textualHint: "The conjunction introduction rule concludes the " <>
                               "formula A ∧ B from the premises A, B."}
              , { sequent: "(A ⊢ C), (B ⊢ C), A ∨ B ⊢ C"
                , textualHint: "To eliminate a disjunction we must show that we can conclude " <>
                               "C regardless of which of A or B holds. We show this by assuming, in turn, A and B respectively and showing that they both will lead to C"}
              , { sequent: "A ⊢ A ∨ B"
                , textualHint: "The left disjunction introduction rule concludes A ∨ B from the " <>
                               "knowledge that A holds."}
              , { sequent: "B ⊢ A ∨ B"
                , textualHint: "The right disjunction introduction rule concludes A ∨ B from the " <>
                               "knowledge that B holds."}
              , { sequent: "A → B, A ⊢ B"
                , textualHint: "If A→B and A are both known facts, the implication elimination " <>
                               "rule can conclude that B also hold."}
              , { sequent: "(A ⊢ B) ⊢ A → B"
                , textualHint: "The implication introduction rule can conclude that A implies B " <>
                               "from a box where the initial assumption is A and the final " <>
                               "conclusion is B."}
              , { sequent: "A,¬A ⊢ ⊥"
                , textualHint: "The negation elimination rule concludes absurdity from the knowledge " <>
                               "that both A and ¬A hold."}
              , { sequent: "(A ⊢ ⊥) ⊢ ¬A"
                , textualHint: "The negation introduction rule can conclude that A does not hold if " <>
                               "from the assumption that A does hold absurdity is concluded."}
              , { sequent: "⊥ ⊢ A"
                , textualHint: "The absurdity elimination rule can conclude anything from the " <>
                               "knowledge of absurdity."}
              , { sequent: "¬¬A ⊢ A"
                , textualHint: "Double negation elimination concludes A from ¬¬A."}
              , { sequent: "A → B, ¬B ⊢ ¬A"
                , textualHint: "The Modus Tollens rule concludes that A does not hold from the " <>
                               "knowledge that A → B and ¬B."}
              , { sequent: "A ⊢ ¬¬A"
                , textualHint: "Double negation introduction concludes ¬¬A from A."}
              , { sequent: "(¬A ⊢ ⊥) ⊢ A"
                , textualHint: "To prove something by contradiction we open a box with the assumption " <>
                               "that that which we want to prove does not hold. If we can conclude " <>
                               "absurdity within the box, the assumption is clearly false and the " <>
                               "opposite must be true."}
              , { sequent: "⊢ A ∨ ¬A"
                , textualHint: "The Law of Excluded Middle concludes that either A must hold or " <>
                               "¬A must hold."}
              , { sequent: "A ⊢ A"
                , textualHint: "At any point we are allowed to reintroduce a premise or an already " <> 
                                "proven formula."}                               
              ]
  
  handleAction :: Int -> H.HalogenM State Int () Output m Unit
  handleAction ix = H.modify_ $ \_ -> Array.index ruleHints ix