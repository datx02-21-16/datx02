module GUI (siteBody) where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import GUI.Config.Text as GCT
import GUI.Proof as GP
import GUI.RulesPanel as RP
import GUI.SettingsPanel as SP
import GUI.StaticElements as SE
import GUI.RawHTML as RawHTML
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

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

  manualModal :: Boolean -> HH.HTML _ _
  manualModal isActive = mkModal "How to use the editor." manualBody isActive
    where
    manualBody = HH.div_
        [ HH.section [ HP.classes [ HH.ClassName "section" ] ]
            [ HH.text $ "This manual will give a brief explanation on how to do proofs in this editor. This will be done by using a step by step \"learn by example\" paradigm by solving basic sequents. When starting the program, you are greeted with the following layout:"]
           , HH.img [ HP.src "src/GUI/img/intro.png"
             ]
           , HH.section [ HP.classes [ HH.ClassName "section"] ] 
              [HH.text $ "The proof column is where the construction of proofs will take place, while the rule column simply lists the inference rules defined in this calculus where clicking on the buttons will give a brief explanation on what they do. The settings column lists this manual whose button you have clicked on because you are here =) , and the shortcut button lists various shortcut keyboard commands used when constructing proofs. An understanding of the shortcuts is needed to be able to know what to type for specific symbols, rules and how to navigate when constructing proofs. In this section, we will for learning purpose write down each command shortcut involved in the process of constructing said proofs. But later when you want to prove sequents that have symbols and rules involved not present in these examples, it is necessary to check the shortcut page to learn how to type them."
             ]
             , HH.section [ HP.classes [ HH.ClassName "section" ] ]
            --[ HH.h1 [ HP.classes [ HH.ClassName "title", HH.ClassName "has-text-centered" ] ] 
               [ HH.strong_ [ HH.text "Example: P∧Q , R ⊢ Q∧R" ] 
               , HH.div_ [HH.text "Initially the proof panel looks like this:" ] ]
               , HH.img [ HP.src "src/GUI/img/Ex1ProofPanel.png" ]
             , HH.section [ HP.classes [ HH.ClassName "section" ] ] 
              [HH.text $ "Always when trying to prove a sequent, an input of the conclusion needs to be supplied to let the editor know what we are trying to conclude. In its initial state, we see that the conclusion part and the row below is highlighted with a red colour which defines some kind of error. For a more thourough explanation on errors, check the error section. We start by adding the conclusion by typing "
                , HH.strong_ [HH.text "QorR " ]
                , HH.text "n the conclusion field and also the premises by executing the following sequence:"
                  , HH.div_ [HH.strong_ [HH.text "PanQ " ] 
                  , HH.text "in the formula field, and " 
                    , HH.strong_ [HH.text "pr " ] 
                    , HH.text "in the rule field."]
                    , HH.div_ [HH.text "Then pressing "
                    , HH.strong_ [HH.text "enter "]
                    , HH.text "will add a new row." ]
                    , HH.div_ [HH.text "And on the current row inputing "
                      , HH.strong_ [HH.text "R "]
                      , HH.text "and "
                        , HH.strong_ [HH.text "pr "]
                        , HH.text "in the formula, respectively the rule field."
                      ]
               ]
               , HH.img [ HP.src "src/GUI/img/Ex1Step1.png" ]
               , HH.section [HP.classes [HH.ClassName "section"]]
                [HH.text "Now it should be clear how to add rows and on each row we have a formula field and a rule field. Next step is to perform a conjunction elimination 2 by typing in the following formula "
                  , HH.strong_ [HH.text "Q "]
                  , HH.text "and rule "
                  , HH.strong_ [HH.text "ane2 1"]
                  , HH.text ":"
                ]
                , HH.img [ HP.src "src/GUI/img/Ex1Step2.png" ]
                , HH.section [HP.classes [HH.ClassName "section"]]
                [HH.text "Lastly we add a new row and type an and introduction by typing in the following formula "
                  , HH.strong_ [HH.text "QanR "]
                  , HH.text "and rule "
                  , HH.strong_ [HH.text "ani 3 2"]
                  , HH.text ":" 
                  ]
                , HH.img [ HP.src "src/GUI/img/Ex1Step3.png" ]
                , HH.section [HP.classes [HH.ClassName "section"]]
                [HH.text "We have successfully proven the sequent P∧Q , R ⊢ Q∧R using the editor. The last row is highlighted with a green color to indicate that the conclusion has been reached and is a valid proof. But what now? No assumptions or maybe an or elimination rule was involved that usually trips people up in introductory logic courses. Lets make it more interesting and prove a harder sequent containing these aspects."
                ]

                , HH.section [ HP.classes [ HH.ClassName "section" ] ]
            --[ HH.h1 [ HP.classes [ HH.ClassName "title", HH.ClassName "has-text-centered" ] ] 
               [ HH.strong_ [ HH.text "Example: (P∨Q)∨R , R ⊢ P∨(Q∨R)" ] 
               , HH.div_ [HH.text "Start by adding the conclusion and the premise by typing " 
                 , HH.strong_ [HH.text "Por(QorR) "]
                 , HH.text "in the conclusion field, and "
                 , HH.strong_ [HH.text "(PorQ)orR"]
                 , HH.text ", in the formula field and rule field at row 1."
                ]
              ]
              , HH.img [ HP.src "src/GUI/img/Ex2Step1.png" ]
              , HH.section [HP.classes [HH.ClassName "section"]]
                [HH.text "Add a new row and type  "
                  , HH.strong_ [HH.text "PorQ "]
                  , HH.text "in the formula field and "
                  , HH.strong_ [HH.text "as "]
                  , HH.text "in the rule field. This opens up a new assumption box with its own scope and assumed formula at the top row. Something to note is that currently when pressing enter, you will add rows inside this box." 
                  ]
              , HH.img [ HP.src "src/GUI/img/Ex2Step2.png" ]
              , HH.section [HP.classes [HH.ClassName "section"]]
               [HH.text "We will now add two new assumptions inside the box we created, because we are going to perform a disjunction elimination at row 2 eventually. To add these assumptions and the inference rules involved, type in the following sequence:  "
                 , HH.div_ [HH.text "Row 3:  Formula field: "
                 , HH.strong_ [HH.text "P "]
                 , HH.text "Rule field:  "
                 , HH.strong_ [HH.text "as "]
                 , HH.div_ [HH.text "Row 4: Formula field: "
                 , HH.strong_ [HH.text "Por(QorR) "]
                 , HH.text "Rule field: "
                 , HH.strong_ [HH.text "ori1 3"]
                 ]
                ]
                , HH.div_ [HH.text "The first assumption box is now finished and we want to add new rows outside of this box. Simply pressing enter doesnt work because it adds new rows inside of it as discussed before. The shortcut we are looking for is to press "
                  , HH.strong_ [HH.text "shift+enter"]
                  , HH.text ". Now we do the second assumption and the inference rules involved by writing the sequence: "
                    , HH.div_ [HH.text "Row 5: Formula field: "
                    , HH.strong_ [HH.text "Q "]
                    , HH.text "Rule field: "
                    , HH.strong_ [HH.text "as "]
                    , HH.div_ [HH.text "Row 6: Formula field: "
                    , HH.strong_ [HH.text "QorR "]
                    , HH.text "Rule field: "
                    , HH.strong_ [HH.text "ori1 5 "]
                    , HH.div_ [HH.text "Row 7: Formula field: "
                    , HH.strong_ [HH.text "Por(QorR) "]
                    , HH.text "Rule field: "
                    ,HH.strong_ [HH.text "ori2 6 "]
                      ]
                    ]
                  ]    
                ]
                , HH.div_ [HH.text "The current state of the proof should now look like this: "]


              ] 
              , HH.img [HP.src "src/GUI/img/Ex2Step3.png"]
              , HH.section [HP.classes [HH.ClassName "section"]]
                [HH.text "Now we press "
                  , HH.strong_ [HH.text "shift+enter "]
                  , HH.text "to get outside of the current box and add a new row. At this row we also input "
                  , HH.strong_ [HH.text "Por(QorR) "]
                  , HH.text "and "
                  , HH.strong_ [HH.text "ore 2 3-4 5-7 "]
                
              ]
              , HH.img [HP.src "src/GUI/img/Ex2Step4.png"]
              , HH.section [HP.classes [HH.ClassName "section"]]
                [HH.text "Add a new assumption box and inference rules inside of it by doing: "
                , HH.div_ [HH.text "Row 9: Formula field: "
                , HH.strong_ [HH.text "R "]
                , HH.text "Rule field: "
                , HH.strong_ [HH.text "as "]
                , HH.div_ [HH.text "Row 10: Formula field: "
                , HH.strong_ [HH.text "QorR "]
                , HH.text "Rule field: "
                , HH.strong_ [HH.text "ori2 9 "]
                , HH.div_ [HH.text "Row 11: Formula field: "
                , HH.strong_ [HH.text "Por(QorR) "]
                , HH.text "Rule field: "
                , HH.strong_ [HH.text "ori2 10 "]
                , HH.text "and lastly "
                , HH.strong_ [HH.text "shift+enter "]
                , HH.text ". It should look like this: "
                  
                    ] 
                  ]
                ]
              ]
              , HH.img [HP.src "src/GUI/img/Ex2Step5.png"]
              , HH.section [HP.classes [HH.ClassName "section"]]
                [HH.text "We finish of the proof by adding the following  "
                , HH.strong_ [HH.text "Por(QorR) "]
                , HH.text "and "
                , HH.strong_ [HH.text "ore 1 , 2-8 , 9-11 "]
                , HH.text "which completes the proof: "
                ]
                , HH.img [HP.src "src/GUI/img/Ex2Step6.png"]
                , HH.section [HP.classes [HH.ClassName "section"]]
                  [HH.text "Phew! This was a little harder than the first one but now you should have enough information about how to navigate and how the editor works by constructing various proofs. Remember to read the shortcuts. if you havnt to learn the various ways of constructing different symbols and rules that havnt been applied on these two example proofs. The best is now to keep on practicing by solving various sequents! Have fun!"]

        ]
        ]]]

  shortcutModal :: HH.HTML _ _
  shortcutModal = mkModal "Syntax shortcuts." SE.shortcutModalBody

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
