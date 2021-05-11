module GUI.StaticElements where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

foreign import manualHTMLString :: String


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
