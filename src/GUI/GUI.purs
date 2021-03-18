module GUI where

import Prelude
import Type.Proxy (Proxy(..))
import GUI.Config.Text as GCT
import GUI.Panels as P
import GUI.StaticElements as SE
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Slots
  = ( panel :: forall query. H.Slot query Void Int )

_panel = Proxy :: Proxy "panel"

siteBody :: forall q i o m. H.Component q i o m
siteBody =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render _ =
    HH.div
      [ HP.classes
          [ HH.ClassName "container" ]
      ]
      [ SE.siteHeader GCT.editorName GCT.editorSlogan
      , HH.section
          [ HP.classes [ HH.ClassName "section" ] ]
          [ HH.div
              [ HP.classes [ HH.ClassName "columns" ] ]
              [ HH.div
                  [ HP.classes [ HH.ClassName "column", HH.ClassName "is-three-quarters" ] ]
                  [ HH.slot _panel 0 P.proofPanel 0 identity ]
              , HH.div
                  [ HP.classes [ HH.ClassName "column" ] ]
                  [ HH.slot _panel 0 P.ruleButtonPanel 0 (identity)
                  , HH.slot _panel 0 P.settingsPanel 0 (identity)
                  ]
              ]
          ]
      , SE.siteFooter
      ]
