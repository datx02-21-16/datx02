module GUI (siteBody) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import GUI.Config.Text as GCT
import GUI.Proof as GP
import GUI.RawHTML as RawHTML
import GUI.RulesPanel as RP
import GUI.SettingsPanel as SP
import GUI.StaticElements as SE
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Type.Proxy (Proxy(..))

foreign import pictureURL :: String -> String

_proof = Proxy :: Proxy "proof"

_settingsPanel = Proxy :: Proxy "settingsPanel"

_ruleButtonPanel = Proxy :: Proxy "ruleButtonPanel"

type Slots
  = ( proof :: GP.Slot Unit
    , ruleButtonPanel :: RP.Slot Unit
    , settingsPanel :: SP.Slot Unit
    , rawHTML :: RawHTML.Slot Int
    )

type State
  = Maybe SP.Modal

data Action
  = ActivateModal SP.Modal
  | CloseModals

siteBody :: forall q i output m. MonadEffect m => H.Component q i output m
siteBody =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = Nothing

  render :: State -> H.ComponentHTML Action Slots m
  render st =
    HH.div
      [ HP.classes
          [ HH.ClassName "container" ]
      ]
      ( [ SE.siteHeader GCT.editorName GCT.editorSlogan
        , HH.section
            [ HP.classes [ HH.ClassName "section" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "columns" ] ]
                [ HH.div
                    [ HP.classes [ HH.ClassName "column", HH.ClassName "is-three-quarters" ] ]
                    [ HH.slot_ _proof unit GP.proof unit ]
                , HH.div
                    [ HP.classes [ HH.ClassName "column" ] ]
                    [ HH.slot_ _ruleButtonPanel unit RP.ruleButtonPanel 0
                    , HH.slot _settingsPanel unit SP.settingsPanel 0 ActivateModal
                    ]
                ]
            ]
        , SE.siteFooter
        ]
          <> ( case st of
                Just m -> [ modal m ]
                Nothing -> []
            )
      )

  modal :: SP.Modal -> HH.HTML _ _
  modal m = case m of
    SP.ManualModal -> manualModal
    SP.ShortcutModal -> shortcutModal

  manualModal :: HH.HTML _ _
  manualModal = mkModal "How to use the editor." manualModalBody

  shortcutModal :: HH.HTML _ _
  shortcutModal = mkModal "Syntax shortcuts." shortcutModalBody

  mkModal :: String -> (HH.HTML _ _) -> HH.HTML _ _
  mkModal title modalBody =
    HH.div [ HP.classes [ HH.ClassName "modal", HH.ClassName "is-active" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "modal-background" ]
          , HE.onClick (\_ -> CloseModals)
          ]
          []
      , HH.div [ HP.classes [ HH.ClassName "modal-card" ] ]
          [ HH.header [ HP.classes [ HH.ClassName "modal-card-head" ] ]
              [ HH.p [ HP.classes [ HH.ClassName "modal-card-title" ] ] [ HH.text title ]
              , HH.button
                  [ HP.classes [ HH.ClassName "delete" ]
                  , ARIA.label "close"
                  , HE.onClick (\_ -> CloseModals)
                  ]
                  []
              ]
          , HH.section [ HP.classes [ HH.ClassName "modal-card-body" ] ] [ modalBody ]
          ]
      ]

  handleAction = case _ of
    ActivateModal m -> H.put (Just m)
    CloseModals -> H.put Nothing

manualModalBody :: forall w. HH.HTML w Action
manualModalBody =
  HH.div [ HP.classes [ HH.ClassName "content" ] ]
    [ HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text $ "This manual will give a brief explanation on how to construct proofs in Logan, by working through two examples. When starting the program, you are greeted with the following layout:" ]
    , HH.img
        [ HP.src $ pictureURL "intro"
        ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.p_ [ HH.text "The interface consists of three main panels, the \"Proof\" panel, the \"About the rules\" panel, and the \"Instructions\" panel." ]
        , HH.p_ [ HH.text "The \"Proof\" panel is where you construct the proofs. It has a field for premises, a field for the conclusions, and a number of lines, each of which has a formula field, a rule field, and, if applicable, a number of argument fields." ]
        , HH.p_ [ HH.text "The \"About the rules\" panel shows the available inference rules. Clicking one of the rules will show additional information about its usage." ]
        , HH.p_ [ HH.text "The \"Instructions\" panel contains this manual and an introduction to the available ", HH.a [ HE.onClick (\_ -> ActivateModal SP.ShortcutModal) ] [ HH.text "shortcuts." ] ]
        ]
    , HH.h4 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-5" ] ]
        [ HH.text "Example: P∧Q , R ⊢ Q∧R" ]
    , HH.p_ [ HH.text "Initially the proof panel looks like this:" ]
    , HH.img [ HP.src $ pictureURL "Ex1ProofPanel" ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text $ "When trying to prove a sequent, we start by adding the premises and conclusion at the top of the proof. We add the premises by typing "
        , HH.strong_ [ HH.text "PanQ, R " ]
        , HH.text "in the premise field, and the conclusion by typing"
        , HH.strong_ [ HH.text " QanR" ]
        , HH.text " in the conclusion field."
        ]
    , HH.img [ HP.src $ pictureURL "Ex1Step1" ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text "The next step is to perform an and-elimination. We do this by typing "
        , HH.strong_ [ HH.text "Q " ]
        , HH.text "in the formula field, and the formula "
        , HH.strong_ [ HH.text "ane2 1" ]
        , HH.text " in the rule and row fields on line 3."
        ]
    , HH.img [ HP.src $ pictureURL "Ex1Step2" ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text "Finally we add a new row, by pressing enter, and perform an and-introduction by typing "
        , HH.strong_ [ HH.text "QanR" ]
        , HH.text " in the formula field, and the rule "
        , HH.strong_ [ HH.text "ani 3 2" ]
        , HH.text " in the rule and row fields on line 4."
        ]
    , HH.img [ HP.src $ pictureURL "Ex1Step3" ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text "We have now proven the sequent P∧Q , R ⊢ Q∧R. The last row is highlighted in green to indicate that the conclusion has been reached and that all steps are valid. We now move on to another example, which demonstrates the use of boxes in a proof."
        ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.h4 [ HP.classes [ HH.ClassName "title", HH.ClassName "is-5" ] ] [ HH.text "Example: (P∨Q)∨R , R ⊢ P∨(Q∨R)" ]
        , HH.div_
            [ HH.text "Start by adding the conclusion and the premise by typing "
            , HH.strong_ [ HH.text "(PorQ)orR" ]
            , HH.text " in the premise field, and "
            , HH.strong_ [ HH.text "Por(QorR) " ]
            , HH.text ", in the conclusion field."
            ]
        ]
    , HH.img [ HP.src $ pictureURL "Ex2Step1" ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text "Type "
        , HH.strong_ [ HH.text "PorQ " ]
        , HH.text "in the formula field and "
        , HH.strong_ [ HH.text "as " ]
        , HH.text "in the rule field on line 2. This opens up a new assumption box with its own scope and assumed formula at the top row. Note that when pressing enter, you will add rows inside the current box."
        ]
    , HH.img [ HP.src $ pictureURL "Ex2Step2" ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text "We will now add two new assumptions inside the box we created, because we are going to perform a or-elimination at row 2 eventually. To add these assumptions and the inference rules involved, type in the following sequence:  "
        , HH.ul_
            [ HH.li_
                [ HH.text "On row 3, type "
                , HH.strong_ [ HH.text "P" ]
                , HH.text " in the formula field, and "
                , HH.strong_ [ HH.text "as" ]
                , HH.text " in the rule field."
                ]
            , HH.li_
                [ HH.text "On row 4, type "
                , HH.strong_ [ HH.text "Por(QorR)" ]
                , HH.text " in the formula field, and "
                , HH.strong_ [ HH.text "ori1 3" ]
                , HH.text " in the rule field."
                ]
            ]
        , HH.p_
            [ HH.text "The first assumption box is now finished and we want to add new rows outside of this box. To add a new row outside the box, press "
            , HH.strong_ [ HH.text "shift+enter" ]
            , HH.text ". Now we make a second assumption and apply the inference rules involved by writing the following: "
            ]
        , HH.ul_
            [ HH.li_
                [ HH.text "On row 5, type "
                , HH.strong_ [ HH.text "Q" ]
                , HH.text " in the formula field, and "
                , HH.strong_ [ HH.text "as" ]
                , HH.text " in the rule field."
                ]
            , HH.li_
                [ HH.text "On row 6, type "
                , HH.strong_ [ HH.text "QorR" ]
                , HH.text " in the formula field, and "
                , HH.strong_ [ HH.text "ori1 5" ]
                , HH.text " in the rule field."
                ]
            , HH.li_
                [ HH.text "On row 7, type "
                , HH.strong_ [ HH.text "Por(QorR)" ]
                , HH.text " in the formula field, and "
                , HH.strong_ [ HH.text "ori2 6" ]
                , HH.text " in the rule field."
                ]
            ]
        , HH.p_ [ HH.text "The proof should now look like this: " ]
        ]
    , HH.img [ HP.src $ pictureURL "Ex2Step3" ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text "Now we press "
        , HH.strong_ [ HH.text "shift+enter " ]
        , HH.text "to get outside of the current box and add a new row. At this row we also input "
        , HH.strong_ [ HH.text "Por(QorR) " ]
        , HH.text "and "
        , HH.strong_ [ HH.text "ore 2 3-4 5-7 " ]
        ]
    , HH.img [ HP.src $ pictureURL "Ex2Step4" ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text "Add a new assumption box and inference rules inside of it by doing: "
        , HH.ul_
            [ HH.li_
                [ HH.text "On row 9, type "
                , HH.strong_ [ HH.text "R" ]
                , HH.text " in the formula field, and "
                , HH.strong_ [ HH.text "as" ]
                , HH.text " in the rule field."
                ]
            , HH.li_
                [ HH.text "On row 10, type "
                , HH.strong_ [ HH.text "QorR" ]
                , HH.text " in the formula field, and "
                , HH.strong_ [ HH.text "ori2 9" ]
                , HH.text " in the rule field."
                ]
            , HH.li_
                [ HH.text "On row 11, type "
                , HH.strong_ [ HH.text "Por(QorR)" ]
                , HH.text " in the formula field, and "
                , HH.strong_ [ HH.text "ori2 10" ]
                , HH.text " in the rule field."
                ]
            ]
        , HH.p_ [ HH.text "The proof should now look like this:" ]
        ]
    , HH.img [ HP.src $ pictureURL "Ex2Step5" ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text "We finish the proof by typing "
        , HH.strong_ [ HH.text "Por(QorR)" ]
        , HH.text " in the formula field, and "
        , HH.strong_ [ HH.text "ore 1 , 2-8 , 9-11 " ]
        , HH.text "in the rule field, which completes the proof."
        ]
    , HH.img [ HP.src $ pictureURL "Ex2Step6" ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text "Phew! This was a little harder than the first one but now you should have enough information about how to use the editor. For more information about the symbols and how to use them, please visit the ", HH.a [ HE.onClick (\_ -> ActivateModal SP.ShortcutModal) ] [ HH.text "shortcut page." ] ]
    ]

shortcutModalBody :: forall w. HH.HTML w Action
shortcutModalBody =
  HH.div_
    [ HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text $ "This section lists various shortcut commands you can "
            <> "type or press with the keyboard when constructing various proofs. For more information on how to use the editor, please visit the "
        , HH.a [ HE.onClick (\_ -> ActivateModal SP.ManualModal) ] [ HH.text "manual." ]
        ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.h1 [ HP.classes [ HH.ClassName "title", HH.ClassName "has-text-centered" ] ] [ HH.text "General" ]
        , HH.table [ HP.classes [ HH.ClassName "table" ] ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th_ [ HH.abbr [ HP.title "Commands" ] [ HH.text "Command" ] ]
                    , HH.th_ [ HH.abbr [ HP.title "Explanation of the commands" ] [ HH.text "Explanation" ] ]
                    , HH.th_ [ HH.abbr [ HP.title "Keyboard shortcuts" ] [ HH.text "Keyboard shortcuts" ] ]
                    ]
                ]
            , HH.tbody []
                [ HH.tr_
                    [ HH.th_ [ HH.text "Add row" ]
                    , HH.td_ [ HH.text "Adds an empty row beneath the current one." ]
                    , HH.td_ [ HH.strong_ [ HH.text "enter" ] ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "Delete row" ]
                    , HH.td_ [ HH.text "Deletes the current row if pressed while having the cursor in the formula field. Note, the formula field needs to be empty for this to work." ]
                    , HH.td_ [ HH.strong_ [ HH.text "backspace" ] ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "Add assumption box" ]
                    , HH.td_ [ HH.text "Opens an assumption box in the current row when you write this command in the rule field." ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "as" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "Ass." ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "Add row outside of assumption box" ]
                    , HH.td_ [ HH.text "While inside an assumption box, pressing enter will add a row in that current box. This command is used to jump out of the box and add a new empty row outside of it." ]
                    , HH.td_ [ HH.strong_ [ HH.text "Shift+Enter" ] ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "Add Premise" ]
                    , HH.td_ [ HH.text "Adds the formula of the current row as a premise when you write this command in the rule field." ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "pr" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "Premise" ]
                        ]
                    ]
                ]
            ]
        ]
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.h1 [ HP.classes [ HH.ClassName "title", HH.ClassName "has-text-centered" ] ] [ HH.text "Symbols" ]
        , HH.table [ HP.classes [ HH.ClassName "table", HH.ClassName "is-fullwidth" ] ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th_ [ HH.abbr [ HP.title "Symbols defined in the logic" ] [ HH.text "Symbol" ] ]
                    , HH.th_ [ HH.abbr [ HP.title "Connectives" ] [ HH.text "Connective" ] ]
                    , HH.th_ [ HH.abbr [ HP.title "Keyboard shortcuts for inputting symbols" ] [ HH.text "Keyboard shortcuts" ] ]
                    ]
                ]
            , HH.tbody []
                [ HH.tr_
                    [ HH.th_ [ HH.text "∧" ]
                    , HH.td_ [ HH.text "Conjunction" ]
                    , HH.td_ [ HH.strong_ [ HH.text "an" ] ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "∨" ]
                    , HH.td_ [ HH.text "Disjunction" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "or" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "v" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "→" ]
                    , HH.td_ [ HH.text "Implication" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "->" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "im" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "¬" ]
                    , HH.td_ [ HH.text "Negation" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "no" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "ne" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "!" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "∀" ]
                    , HH.td_ [ HH.text "Universal, for all" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "fa" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "fo" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "∃" ]
                    , HH.td_ [ HH.text "Existential, there exist" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "ex" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "te" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "⊥" ]
                    , HH.td_ [ HH.text "Bottom / Contradiction" ]
                    , HH.td_ [ HH.strong_ [ HH.text "bo" ] ]
                    ]
                ]
            ]
        ]
    ]
