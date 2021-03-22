module GUI.SettingsPanel where

import Prelude (identity)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import GUI.Config.Text as GCT

settingsPanel :: forall query input output m. H.Component query input output m
settingsPanel =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render state =
    HH.div
      [ HP.classes [ HH.ClassName "panel", HH.ClassName "is-primary" ] ]
      [ HH.p
          [ HP.classes [ HH.ClassName "panel-heading" ] ]
          [ HH.text "Settings" ]
      , HH.div
          [ HP.classes [ HH.ClassName "panel-block" ] ]
          [ HH.text GCT.panelNotImplemented ]
      ]