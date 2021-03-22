module GUI where

import Prelude
import Type.Proxy (Proxy(..))
import GUI.Config.Text as GCT
import GUI.Panels as P
import GUI.StaticElements as SE
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Effect.Class (class MonadEffect)

type RuleSlots = ( button     ::               H.Slot P.Query P.Output Int 
                 , panel      :: forall query. H.Slot query Void Int
                 , proofPanel ::               H.Slot P.Query Void Int
                 )

_proofPanel = Proxy :: Proxy "proofPanel"
_panel      = Proxy :: Proxy "panel"
_button     = Proxy :: Proxy "button"

data Action = HandleButton P.Output

siteBody :: forall q i output m. MonadEffect m => H.Component q i output m
siteBody =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction}
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
                  [ HH.slot _proofPanel 0 P.proofPanel 0 identity ]
              , HH.div
                  [ HP.classes [ HH.ClassName "column" ] ]
                  [ HH.slot _button 1 P.ruleButtonPanel 0 HandleButton -- P.Output -> Action
                  , HH.slot _panel 2 P.settingsPanel 0 identity
                  ]
              ]
          ]
      , SE.siteFooter
      ]
  
--  handleAction :: forall state. Action -> H.HalogenM state Action RuleSlots output m Unit
  handleAction = case _ of
      HandleButton output -> do
        H.tell _proofPanel 0 (P.Tell output) -- a -> Query a
        H.modify_ \st -> st
