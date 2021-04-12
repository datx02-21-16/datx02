module GUI.RulesPanel where

import Prelude (Unit, Void, discard, identity, pure, ($), (<>), show)
import Type.Proxy (Proxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import GUI.Proof as GP
import GUI.Rules (RuleType(..), rules)
import Data.Tuple
import Data.Eq
import Data.Maybe
import Data.Array as Array
import Data.Foldable
import Data.EuclideanRing

type Slots
  = ( proofPanel :: forall query. H.Slot query Void Int
    , proof :: forall output query. H.Slot query output Int
    )

_proofPanel = Proxy :: Proxy "proofPanel"

_proof = Proxy :: Proxy "proof"

proofPanel :: forall input output query m. MonadEffect m => H.Component query input output m
proofPanel =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render :: forall state action. state -> H.ComponentHTML action Slots m
  render _ =
    HH.div
      [ HP.classes [ HH.ClassName "panel", HH.ClassName "is-primary" ] ]
      [ HH.p
          [ HP.classes [ HH.ClassName "panel-heading" ] ]
          [ HH.text "Proof" ]
      , HH.div
          [ HP.classes [ HH.ClassName "panel-block" ] ]
          [ HH.slot_ _proof 0 GP.proof {} ]
      ]

type State
  = Maybe RuleType

ruleButtonPanel :: forall query output m. MonadEffect m => H.Component query Int output m
ruleButtonPanel =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = Nothing

  render st =
    HH.div
      [ HP.classes [ HH.ClassName "panel", HH.ClassName "is-primary" ] ]
      $ [ HH.p
            [ HP.classes [ HH.ClassName "panel-heading" ] ]
            [ HH.text "Rules" ]
        ]
      <> [ createButtons ]
      <> [ hintBox st ]

-- I don't understand why the buttons does not fill out the whole space?
  createButtons =
    HH.div [ HP.classes [ HH.ClassName "columns" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "column", HH.ClassName "is-flex-grow-1" 
                            , HH.ClassName "is-justify-content-center" ] ] leftb
      , HH.div [ HP.classes [ HH.ClassName "column", HH.ClassName "is-flex-grow-1"
                            , HH.ClassName "is-justify-content-center" ] ] rightb
      ]
    where
    (Tuple leftb rightb) =
      foldl
        ( \(Tuple c1 c2) (Tuple t i) ->
            if eq (i `mod` 2) 0 then
              (Tuple (Array.snoc c1 (createButton i t)) c2)
            else
              (Tuple c1 (Array.snoc c2 (createButton i t)))
        )
        (Tuple [] [])
        (Array.zip rules (Array.range 0 (Array.length rules)))

  createButton i rule =
    HH.button
      [ HP.classes [ HH.ClassName "button" ]
      , HP.type_ HP.ButtonSubmit
      , HE.onClick $ \_ -> rule
      ]
      [ HH.text (show rule) ]

  hintBox st =
    HH.div
      [ HP.classes [ HH.ClassName "box" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "has-text-centered" ] ]
          [ HH.text $ maybe hintString sequent st ]
      , HH.br_
      , HH.div
          [ HP.classes [ HH.ClassName "has-text-centered" ] ]
          [ HH.text $ maybe "" textualHint st ]
      ]
    where
    hintString :: String
    hintString =
      "Please click one of the rules above to "
        <> "get a description of the rule."

  sequent :: RuleType -> String
  sequent r = case r of
    RtPremise -> "⊢ A"
    -- Not sure if this is correct or how to (or even if we should) represent assumption as a sequent?
    RtAssumption -> "(⊢ A)"
    AndElim1 -> "A ∧ B ⊢ A"
    AndElim2 -> "A ∧ B ⊢ B"
    AndIntro -> "A,B ⊢ A ∧ B"
    OrElim -> "(A ⊢ C), (B ⊢ C), A ∨ B ⊢ C"
    OrIntro1 -> "A ⊢ A ∨ B"
    OrIntro2 -> "B ⊢ A ∨ B"
    ImplElim -> "A → B, A ⊢ B"
    ImplIntro -> "(A ⊢ B) ⊢ A → B"
    NegElim -> "A,¬A ⊢ ⊥"
    NegIntro -> "(A ⊢ ⊥) ⊢ ¬A"
    BottomElim -> "⊥ ⊢ A"
    DoubleNegElim -> "¬¬A ⊢ A"
    ModusTollens -> "A → B, ¬B ⊢ ¬A"
    DoubleNegIntro -> "A ⊢ ¬¬A"
    PBC -> "(¬A ⊢ ⊥) ⊢ A"
    LEM -> "⊢ A ∨ ¬A"

  textualHint :: RuleType -> String
  textualHint r = case r of
    RtPremise ->
      "A premise is something that is assumed to be universally true, "
        <> "and therefore need no justification."
    RtAssumption ->
      "An assumption introduces a new fact without justification, "
        <> "but does so inside a new scope. The assumption or any conclusions "
        <> "drawn from it is not allowed to escape this scope."
    AndElim1 ->
      "The left conjunction elimination rule concludes "
        <> "the formula A from the premise A∧B."
    AndElim2 ->
      "The right conjunction elimination rule concludes "
        <> "the formula B from the premise A∧B."
    AndIntro ->
      "The conjunction introduction rule concludes the "
        <> "formula A ∧ B from the premises A, B."
    OrElim ->
      "To eliminate a disjunction we must show that we can conclude "
        <> "C regardless of which of A or B holds. We show this by assuming, "
        <> "in turn, A and B respectively and showing that they both will lead to C"
    OrIntro1 ->
      "The left disjunction introduction rule concludes A ∨ B from the "
        <> "knowledge that A holds."
    OrIntro2 ->
      "The right disjunction introduction rule concludes A ∨ B from the "
        <> "knowledge that B holds."
    ImplElim ->
      "If A→B and A are both known facts, the implication elimination "
        <> "rule can conclude that B also hold."
    ImplIntro ->
      "The implication introduction rule can conclude that A implies B "
        <> "from a box where the initial assumption is A and the final "
        <> "conclusion is B."
    NegElim ->
      "The negation elimination rule concludes absurdity from the knowledge "
        <> "that both A and ¬A hold."
    NegIntro ->
      "The negation introduction rule can conclude that A does not hold if "
        <> "from the assumption that A does hold absurdity is concluded."
    BottomElim ->
      "The absurdity elimination rule can conclude anything from the "
        <> "knowledge of absurdity."
    DoubleNegElim -> "Double negation elimination concludes A from ¬¬A."
    ModusTollens ->
      "The Modus Tollens rule concludes that A does not hold from the "
        <> "knowledge that A → B and ¬B."
    DoubleNegIntro -> "Double negation introduction concludes ¬¬A from A."
    PBC ->
      "To prove something by contradiction we open a box with the assumption "
        <> "that that which we want to prove does not hold. If we can conclude "
        <> "absurdity within the box, the assumption is clearly false and the "
        <> "opposite must be true."
    LEM ->
      "The Law of Excluded Middle concludes that either A must hold or "
        <> "¬A must hold."

  handleAction :: forall output. RuleType -> H.HalogenM State RuleType () output m Unit
  handleAction rule = H.modify_ $ \_ -> Just rule
