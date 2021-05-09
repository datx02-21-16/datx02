module GUI.StaticElements where

import Prelude
import Effect.Class (class MonadEffect)
import Type.Proxy (Proxy(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import GUI.RawHTML as RawHTML

foreign import manualHTMLString :: String

_rawHTML = Proxy :: Proxy "rawHTML"

-- | Main page header.
siteHeader :: forall w i. String -> String -> HH.HTML w i
siteHeader title subtitle =
  HH.section
    [ HP.classes [ HH.ClassName "hero", HH.ClassName "is-small", HH.ClassName "is-primary" ] ]
    [ HH.div
        [ HP.class_ (HH.ClassName "hero-body") ]
        [ HH.p
            [ HP.classes [ HH.ClassName "title", HH.ClassName "is-1" ] ]
            [ HH.text title ]
        , HH.p
            [ HP.classes [ HH.ClassName "subtitle" ] ]
            [ HH.text subtitle ]
        ]
    ]

-- | Footer for the page.
siteFooter :: forall w i. HH.HTML w i
siteFooter =
  HH.footer [ HP.classes [ HH.ClassName "footer" ] ]
    [ HH.div [ HP.classes [ HH.ClassName "content", HH.ClassName "has-text-centered" ] ]
        [ HH.p_ [ HH.text "Licensed under ", HH.a [ HP.href "https://opensource.org/licenses/GPL-3.0" ] [ HH.text "GPLv3" ] ]
        ]
    ]

manualModalBody :: forall w i. HH.HTML w i
manualModalBody = HH.div_
        [ HH.section [ HP.classes [ HH.ClassName "section" ] ]
            [ HH.text $ "This manual will give a brief explanation on how to do proofs in this editor. This will be done by using a step by step \"learn by example\" paradigm by solving basic sequents. When starting the program, you are greeted with the following layout:"]
           , HH.img [ HP.src "src/GUI/img/intro.png"
             ]
           , HH.section [ HP.classes [ HH.ClassName "section"] ] 
              [HH.text $ "The proof column is where the construction of proofs will take place, while the rule column simply lists the inference rules defined in this calculus where clicking on the buttons will give a brief explanation on what they do. The settings column lists this manual whose button you have clicked on because you are here =) , and the shortcut button lists various shortcut keyboard commands used when constructing proofs. An understanding of the shortcuts is needed to be able to know what to type for specific symbols, rules and how to navigate when constructing proofs. In this section, we will for learning purpose write down each command shortcut involved in the process of constructing said proofs. But later when you want to prove sequents that have symbols and rules involved not present in these examples, it is necessary to check the shortcut page to learn how to type them."
             ]
             , HH.section [ HP.classes [ HH.ClassName "section" ] ]
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

shortcutModalBody :: forall w i. HH.HTML w i
shortcutModalBody =
  HH.div_
    [ HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.text $ "This section lists various shortcut commands you can "
            <> "type or press with the keyboard when constructing various proofs."
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
                        , HH.strong_ [ HH.text "imp" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "¬" ]
                    , HH.td_ [ HH.text "Negation" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "not" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "neg" ]
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
                    , HH.td_
                        [ HH.strong_ [ HH.text "bo" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "con" ]
                        ]
                    ]
                ]
            ]
        ]
    ]

{- This might be unnecessary information to include.
    , HH.section [ HP.classes [ HH.ClassName "section" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "title", HH.ClassName "has-text-centered" ] ]
            [ HH.text "Rules" ]
        , HH.table [ HP.classes [ HH.ClassName "table", HH.ClassName "is-fullwidth" ] ]
            [ HH.thead_
                [ HH.tr_
                    [ HH.th_ [ HH.abbr [ HP.title "Symbolic representation of the rules" ] [ HH.text "Symbol" ] ]
                    , HH.th_ [ HH.abbr [ HP.title "Rule" ] [ HH.text "Rule" ] ]
                    , HH.th_ [ HH.abbr [ HP.title "Keyboard shortcuts typed in the rule field when constructing proofs" ] [ HH.text "Keyboard shortcuts" ] ]
                    ]
                ]
            , HH.tbody []
                [ HH.tr_
                    [ HH.th_ [ HH.text "∧i" ]
                    , HH.td_ [ HH.text "Conjunction introduction" ]
                    , HH.td_ [ HH.strong_ [ HH.text "ani" ] ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "∧e", HH.sub_ [ HH.text "1" ] ]
                    , HH.td_ [ HH.text "Conjunction elimination 1" ]
                    , HH.td_ [ HH.strong_ [ HH.text "ane1" ] ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "∧e", HH.sub_ [ HH.text "2" ] ]
                    , HH.td_ [ HH.text "Conjunction elimination 2" ]
                    , HH.td_ [ HH.strong_ [ HH.text "ane2" ] ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "∨i", HH.sub_ [ HH.text "1" ] ]
                    , HH.td_ [ HH.text "Disjunction introduction 1" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "ori1" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "∨i1" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "∨i", HH.sub_ [ HH.text "2" ] ]
                    , HH.td_ [ HH.text "Disjunction introduction 2" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "ori2" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "∨i2" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "∨e" ]
                    , HH.td_ [ HH.text "Disjunction elimination" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "ore" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "∨e" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "→i" ]
                    , HH.td_ [ HH.text "Implication introduction" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "->i" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "impi" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "→e" ]
                    , HH.td_ [ HH.text "Implication elimination" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "->e" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "impe" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "¬i" ]
                    , HH.td_ [ HH.text "Negation introduction" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "note" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "nege" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "!e" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "¬e" ]
                    , HH.td_ [ HH.text "Negation elimination" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "noti" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "negi" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "!i" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "⊥e" ]
                    , HH.td_ [ HH.text "Bottom elimination" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "boe" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "cone" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "¬¬e" ]
                    , HH.td_ [ HH.text "Double negation elimination" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "notnote" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "negnege" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "!!e" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "¬¬i" ]
                    , HH.td_ [ HH.text "Double negation introduction" ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "notnoti" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "negnegi" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "!!i" ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "MT" ]
                    , HH.td_ [ HH.text "Modus Tollens" ]
                    , HH.td_ [ HH.strong_ [ HH.text "mt" ] ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "PBC" ]
                    , HH.td_ [ HH.text "Proof by Contraction" ]
                    , HH.td_ [ HH.strong_ [ HH.text "pbc" ] ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "LEM" ]
                    , HH.td_ [ HH.text "Law of Excluded Middle" ]
                    , HH.td_ [ HH.strong_ [ HH.text "lem" ] ]
                    ]
                ]
            ]
        ]
    ]
-}
