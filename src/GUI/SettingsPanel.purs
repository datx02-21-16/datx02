module GUI.SettingsPanel where

import GUI.Config.Text as GCT
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (identity)

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
          [ HH.a 
                [HP.href "staticAssets/manual.html" , HP.target "_blank"] 
                [HH.button 
                     [HP.classes[HH.ClassName "button"]] 
                     [HH.text "Manual"]]
           , HH.a 
                [HP.href "staticAssets/shortcuts.html" , HP.target "_blank"] 
                [HH.button 
                     [HP.classes[HH.ClassName "button"]] 
                     [HH.text "Shortcuts"]]          
          ]
      ]