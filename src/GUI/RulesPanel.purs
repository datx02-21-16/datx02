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

foreign import pictureURL :: String -> String

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
            [ HH.text "About the rules" ]
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
      [ HP.classes [ H.ClassName "box", H.ClassName "has-text-centered" ] ]
      $ maybe [ HH.text "Please click one of the rules above to get a description of the rule." ]
          ( \rule ->
              [ HH.img [ HP.src $ pictureURL (ruleImageName rule) ]
              , HH.p_ [ HH.strong_ [ HH.text $ "Shortcut: " <> shortcut rule ] ]
              , HH.p [ HP.classes [ HH.ClassName "has-text-left" ] ] [ HH.text $ textualHint rule ]
              ]
          )
          st

  shortcut :: RuleType -> String
  shortcut = case _ of
    RtPremise -> "pr"
    RtAssumption -> "as"
    AndElim1 -> "ane1"
    AndElim2 -> "ane2"
    AndIntro -> "ani"
    OrElim -> "ore"
    OrIntro1 -> "ori1"
    OrIntro2 -> "ori2"
    ImplElim -> "ime"
    ImplIntro -> "imi"
    NegElim -> "noe/nee"
    NegIntro -> "noi/nei"
    BottomElim -> "boe/coe"
    DoubleNegElim -> "nonoe/nenee"
    ModusTollens -> "mt"
    DoubleNegIntro -> "nonoi/nenee"
    PBC -> "pbc"
    LEM -> "lem"
    RtCopy -> "cp"

  {-RtFresh -> "fr"
    ForallElim -> "foe/fae"
    ForallIntro -> "foi/fai"
    ExistsElim -> "exe/tee"
    ExistsIntro -> "exi/tei"
    EqElim -> "eqe"
    EqIntro -> "eqi"
    -}
  ruleImageName :: RuleType -> String
  ruleImageName = case _ of
    RtPremise -> "premise"
    RtAssumption -> "assumption"
    AndElim1 -> "and-elim1"
    AndElim2 -> "and-elim2"
    AndIntro -> "and-intro"
    OrElim -> "or-elim"
    OrIntro1 -> "or-intro1"
    OrIntro2 -> "or-intro2"
    ImplElim -> "implication-elim"
    ImplIntro -> "implication-intro"
    NegElim -> "negation-elim"
    NegIntro -> "negation-intro"
    BottomElim -> "bottom-elim"
    DoubleNegElim -> "double-negation-elim"
    ModusTollens -> "MT"
    DoubleNegIntro -> "double-negation-intro"
    PBC -> "PBC"
    LEM -> "LEM"
    RtCopy -> "copy"

  {-RtFresh -> "fresh"
    ForallElim -> "forall-elim"
    ForallIntro -> "forall-intro"
    ExistsElim -> "exist-elim"
    ExistsIntro -> "exist-intro"
    EqElim -> "eq-elim"
    EqIntro -> "eq-intro"
    -}
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
        <> "the formula φ from the premise φ∧ψ."
    AndElim2 ->
      "The right conjunction elimination rule concludes "
        <> "the formula ψ from the premise φ∧ψ."
    AndIntro ->
      "The conjunction introduction rule concludes the "
        <> "formula φ ∧ ψ from the premises φ, ψ."
    OrElim ->
      "To eliminate a disjunction we must show that we can conclude "
        <> "χ regardless of which of φ or ψ holds. We show this by assuming, "
        <> "in turn, φ and ψ respectively and showing that they both will lead to χ"
    OrIntro1 ->
      "The left disjunction introduction rule concludes φ ∨ ψ from the "
        <> "knowledge that φ holds."
    OrIntro2 ->
      "The right disjunction introduction rule concludes φ ∨ ψ from the "
        <> "knowledge that ψ holds."
    ImplElim ->
      "If φ→ψ and φ are both known facts, the implication elimination "
        <> "rule can conclude that ψ also hold."
    ImplIntro ->
      "The implication introduction rule can conclude that φ implies ψ "
        <> "from a box where the initial assumption is φ and the final "
        <> "conclusion is ψ."
    NegElim ->
      "The negation elimination rule concludes absurdity from the knowledge "
        <> "that both φ and ¬φ hold."
    NegIntro ->
      "The negation introduction rule can conclude that φ does not hold if "
        <> "from the assumption that φ does hold absurdity is concluded."
    BottomElim ->
      "The absurdity elimination rule can conclude anything from the "
        <> "knowledge of absurdity."
    DoubleNegElim -> "Double negation elimination concludes φ from ¬¬φ."
    ModusTollens ->
      "The Modus Tollens rule concludes that φ does not hold from the "
        <> "knowledge that φ → B and ¬B."
    DoubleNegIntro -> "Double negation introduction concludes ¬¬φ from φ."
    PBC ->
      "To prove something by contradiction we open a box with the assumption "
        <> "that that which we want to prove does not hold. If we can conclude "
        <> "absurdity within the box, the assumption is clearly false and the "
        <> "opposite must be true."
    LEM ->
      "The Law of Excluded Middle concludes that either φ must hold or "
        <> "¬φ must hold."
    RtCopy -> "A proven formula can always be copied if it is in scope."
    _ -> "No explanation"

  {-RtFresh ->
      "A fresh variable is a new variable which does not occur anywhere "
        <> "outside its box. It can be thought of as an arbitrary term."
    ForallElim -> "You can replace the x in φ with any term t as long as " 
        <> "t is free for x in φ."
    ForallIntro -> "If you start with a fresh variable and are able to prove "
        <> "some formula φ[x0/x] with x0 in it then you can derive ∀xφ."
    ExistsElim -> "You use x0 as a generic value representing all possible "
        <> "values. If [x0/x] allows you to prove something which does not "
        <> "mention x0 then this must be true no matter which specific value of "  
        <> "x0 makes φ[x0/x]. Note that x0 cannot occur outside the scope of "
        <> "the assumption."
    ExistsIntro -> "If you have φ[t/x] for some term t that is free for x in " 
        <> "φ, then you may deduce ∃xφ."
    EqElim ->
      "This rule allows us to substitute equals with equals repeatedly. It "
        <> "comes with the side condition that t1 and t2 has to be free for x in φ"
    EqIntro -> "Equality in terms of computation results. It can only be "
        <> "applied to terms."
    -}
  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction action = case action of
    SetRule rule -> H.put $ Just rule
    ClearRule -> H.put Nothing
