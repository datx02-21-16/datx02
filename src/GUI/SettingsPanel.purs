module GUI.SettingsPanel (Slot, Modal(..), settingsPanel) where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (identity)

type Slot id
  = forall query. H.Slot query Modal id

type Output
  = Modal

data Modal
  = ManualModal
  | ShortcutModal

settingsPanel :: forall query input m. H.Component query input Output m
settingsPanel =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  render state =
    HH.div
      [ HP.classes [ HH.ClassName "panel", HH.ClassName "is-primary" ] ]
      [ HH.p
          [ HP.classes [ HH.ClassName "panel-heading" ] ]
          [ HH.text "Instructions" ]
      , HH.div
          [ HP.classes [ HH.ClassName "panel-block" ] ]
          [ HH.button
              [ HP.classes [ HH.ClassName "button" ]
              , HE.onClick (\_ -> ManualModal)
              ]
              [ HH.text "Manual" ]
          , HH.button
              [ HP.classes [ HH.ClassName "button" ]
              , HE.onClick (\_ -> ShortcutModal)
              ]
              [ HH.text "Shortcuts" ]
          ]
      ]

  handleAction action = H.raise action
