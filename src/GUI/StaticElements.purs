module GUI.StaticElements where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

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

-- | Footer for the page
siteFooter :: forall t1 t2. HH.HTML t2 t1
siteFooter = HH.section_ [ HH.p_ [ HH.text "Licensed under GPLv3" ] ]
