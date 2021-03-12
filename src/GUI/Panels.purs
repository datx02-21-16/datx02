module GUI.Panels where

import Prelude
import GUI.Config.Text as GCT
import GUI.Proof as GP
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Panel
  = H.Component HH.HTML

proofPanel :: forall t11 t12 t31 t34. Panel t34 t31 t12 t11
proofPanel =
  H.mkComponent
    { initialState: \_ -> unit
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render _ =
    HH.div
      [ HP.classes [ HH.ClassName "panel", HH.ClassName "is-primary" ] ]
      [ HH.p
          [ HP.classes [ HH.ClassName "panel-heading" ] ]
          [ HH.text "Proof" ]
      , HH.div
          [ HP.classes [ HH.ClassName "panel-block" ] ]
          [ HH.text GCT.panelNotImplemented ]
      ]

ruleButtonPanel :: forall t11 t12 t31 t34. Panel t34 t31 t12 t11
ruleButtonPanel =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render _ =
    HH.div
      [ HP.classes [ HH.ClassName "panel", HH.ClassName "is-primary" ] ]
      [ HH.p
          [ HP.classes [ HH.ClassName "panel-heading" ] ]
          [ HH.text "Rules" ]
      , HH.div
          [ HP.classes [ HH.ClassName "panel-block" ] ]
          [ HH.text GCT.panelNotImplemented ]
      ]

settingsPanel :: forall t11 t12 t31 t34. Panel t34 t31 t12 t11
settingsPanel =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render _ =
    HH.div
      [ HP.classes [ HH.ClassName "panel", HH.ClassName "is-primary" ] ]
      [ HH.p
          [ HP.classes [ HH.ClassName "panel-heading" ] ]
          [ HH.text "Settings" ]
      , HH.div
          [ HP.classes [ HH.ClassName "panel-block" ] ]
          [ HH.text GCT.panelNotImplemented ]
      ]
