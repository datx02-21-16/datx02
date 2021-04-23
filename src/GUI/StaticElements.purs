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

manualModalBody :: forall r action m. MonadEffect m => HH.ComponentHTML action ( rawHTML :: RawHTML.Slot Int | r ) m
manualModalBody = HH.slot_ _rawHTML 0 RawHTML.component manualHTMLString

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
