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
