module GUI.RulesPanel (Slot, ruleButtonPanel) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect.Class (class MonadEffect)
import GUI.Rules (RuleType(..), rules)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot id
  = forall query output. H.Slot query output id

type State
  = Maybe RuleType

data Action
  = SetRule RuleType
  | ClearRule

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
    where
    createButtons = HH.div [ HP.classes [ HH.ClassName "columns", HH.ClassName "is-multiline", HH.ClassName "is-gapless" ] ] (map createButton rules)

    createButton rule =
      HH.div [ HP.classes [ HH.ClassName "column", HH.ClassName "is-half" ] ]
        [ HH.button
            [ HP.classes ([ HH.ClassName "button", HH.ClassName "is-fullwidth" ] <> maybe [] (\rt -> if (rt == rule) then [ HH.ClassName "is-primary" ] else []) st)
            , HP.type_ HP.ButtonSubmit
            , HE.onClick $ \_ -> if st == (Just rule) then ClearRule else SetRule rule
            ]
            [ HH.text (show rule) ]
        ]

  hintBox st =
    HH.div
      [ HP.classes [ HH.ClassName "box" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "has-text-centered" ] ]
          [ maybe (HH.text hintString) sequent st ]
      --            HH.text $ maybe hintString sequent st ]
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

  --  sequent :: RuleType -> String
  sequent r = case r of
    RtPremise -> HH.img [ HP.src $ image "premise.png" ]
    RtAssumption -> HH.img [ HP.src $ image "assumption.png" ]
    AndElim1 -> HH.img [ HP.src $ image "and-elim1.png" ]
    AndElim2 -> HH.img [ HP.src $ image "and-elim2.png" ]
    AndIntro -> HH.img [ HP.src $ image "and-intro.png" ]
    OrElim -> HH.img [ HP.src $ image "or-elim.png" ]
    OrIntro1 -> HH.img [ HP.src $ image "or-intro1.png" ]
    OrIntro2 -> HH.img [ HP.src $ image "or-intro2.png" ]
    ImplElim -> HH.img [ HP.src $ image "implication-elim.png" ]
    ImplIntro -> HH.img [ HP.src $ image "implication-intro.png" ]
    NegElim -> HH.img [ HP.src $ image "negation-elim.png" ]
    NegIntro -> HH.img [ HP.src $ image "negation-intro.png" ]
    BottomElim -> HH.img [ HP.src $ image "bottom-elim.png" ]
    DoubleNegElim -> HH.img [ HP.src $ image "double-negation-elim.png" ]
    ModusTollens -> HH.img [ HP.src $ image "MT.png" ]
    DoubleNegIntro -> HH.img [ HP.src $ image "double-negation-intro.png" ]
    PBC -> HH.img [ HP.src $ image "PBC.png" ]
    LEM -> HH.img [ HP.src $ image "LEM.png" ]
    RtCopy -> HH.img [ HP.src $ image "copy.png" ]
    where
    image :: String -> String
    image str = "src/GUI/Pictures/" <> str

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
    RtCopy -> "A proven formula can always be copied if it is in scope."
    _ -> "No explanation"

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction action = case action of
    SetRule rule -> H.put $ Just rule
    ClearRule -> H.put Nothing
