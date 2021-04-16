module GUI.StaticElements where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude (($), (<>))

-- | Main page header.
siteHeader :: forall t1 t2. String -> String -> HH.HTML t2 t1
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

manualModalBody :: HH.HTML _ _
manualModalBody = HH.text lorem

shortcutModalBody :: HH.HTML _ _
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
                    , HH.td_ [ HH.text "Add an empty row beneath a current one." ]
                    , HH.td_ [ HH.strong_ [ HH.text "enter" ] ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "Delete row" ]
                    , HH.td_ [ HH.text "Delete a current row. Note, the formula field in the proof panel needs to be empty to remove a row. If it is not empty, then pressing backspace until the formula is gone will delete the row." ]
                    , HH.td_ [ HH.strong_ [ HH.text "backspace" ] ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "Add Assumption box" ]
                    , HH.td_ [ HH.text "Opens an assumption box at a specific row when inputting the shortcut in the rule field." ]
                    , HH.td_
                        [ HH.strong_ [ HH.text "as" ]
                        , HH.text ", "
                        , HH.strong_ [ HH.text "Ass." ]
                        ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "Add row outside of assumption box" ]
                    , HH.td_ [ HH.text "When inside an assumption box, pressing enter will add a row in that current box. This command is used to jump outside of the box and add a row outside of it." ]
                    , HH.td_ [ HH.strong_ [ HH.text "Alt+Enter" ] ]
                    ]
                , HH.tr_
                    [ HH.th_ [ HH.text "Add Premise" ]
                    , HH.td_ [ HH.text "Adds a Premise in the rule field." ]
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

-- Placeholder
lorem :: String
lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce lobortis nulla a eros ullamcorper, vitae tincidunt tortor auctor. Quisque quis sapien lobortis, rutrum diam quis, porttitor urna. Donec tristique ut quam sit amet luctus. In finibus est feugiat maximus blandit. Nunc vel accumsan neque, sed tristique justo. Vestibulum sit amet vulputate felis, vitae malesuada dolor. Proin odio lacus, iaculis sed luctus et, pellentesque eget tortor. Curabitur maximus lorem massa. Donec ornare pretium justo eu malesuada. Aenean sed libero molestie, consequat justo eu, dapibus nisi. Cras vestibulum sapien at risus luctus, id fermentum massa mattis. Aliquam nec nulla dui. Praesent laoreet enim suscipit lorem facilisis, volutpat laoreet turpis aliquam. Vivamus cursus efficitur diam quis cursus. Integer tempus convallis metus vitae tincidunt. Vestibulum maximus maximus euismod. Fusce imperdiet et tellus luctus pellentesque. In eu egestas velit. Phasellus luctus lectus vitae justo venenatis accumsan. Suspendisse dapibus quam vitae pharetra gravida. Vivamus sit amet elementum tellus, sit amet maximus sem. Etiam cursus pellentesque elit, sit amet volutpat tellus maximus quis. Sed molestie tortor neque, nec sollicitudin turpis luctus sit amet. Mauris eu massa at nisl condimentum congue. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec tincidunt dignissim mauris, facilisis vehicula metus mattis id. Donec quis risus ipsum. Praesent faucibus faucibus ullamcorper. Cras eu fringilla magna, ac sodales augue. Aliquam aliquet congue velit, vitae vulputate nunc elementum ut. Cras ultricies consectetur commodo. Proin rhoncus quam sed leo blandit, et feugiat justo iaculis. Nam odio odio, tincidunt id augue vel, vulputate gravida purus. Sed semper risus nec nisi aliquet iaculis quis non augue. Maecenas quis turpis ut mauris ultrices facilisis. In quis est accumsan erat consequat placerat vel pretium enim. Nunc accumsan, neque eget dictum malesuada, ante metus rhoncus lacus, ac volutpat nunc magna rutrum felis. Donec quis turpis at tellus mattis porta a eu dolor. Quisque venenatis lacus a ligula lacinia dignissim. Aliquam placerat mi a ullamcorper euismod. Nullam sit amet nisl neque. Phasellus nec lobortis nunc. Quisque feugiat volutpat aliquet. Nunc ut pulvinar velit. Praesent sed tellus erat. Nulla volutpat ipsum nec ipsum ornare, id condimentum nisl varius. Curabitur nec purus ac leo rutrum fringilla. Donec eu mi ut lectus dapibus sodales. Phasellus hendrerit facilisis ornare. Donec mi augue, volutpat quis semper non, aliquet a elit. Phasellus commodo sagittis velit, a bibendum quam suscipit eu. Suspendisse potenti. Nullam posuere neque euismod, vestibulum purus vitae, fermentum orci. Curabitur elementum dui eu quam consequat maximus. Aliquam quis erat tincidunt quam ornare tincidunt. Integer viverra efficitur suscipit. Sed volutpat lectus vitae ultricies consequat. Cras ut porttitor magna. Vestibulum tortor augue, mollis a tempus nec, suscipit vitae lectus. Aliquam a bibendum turpis. Pellentesque id tempor orci. Duis elit massa, accumsan eu turpis sed, consectetur molestie ipsum. Vivamus eu commodo lectus. Donec facilisis mi id lectus auctor tempus. Curabitur nec mauris nec lorem varius consequat. Fusce tristique ligula vel lectus fermentum, nec laoreet est dapibus. In ultricies mauris nibh, nec ultrices tellus viverra maximus. Ut a mi nec mi scelerisque facilisis sit amet eget elit. Curabitur semper nulla."
