module GUI where

import Prelude
import Type.Proxy (Proxy(..))
import GUI.Config.Text as GCT
import GUI.RulesPanel as RP
import GUI.SettingsPanel as SP
import GUI.ProofPanel as PP
import GUI.StaticElements as SE
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Effect.Class (class MonadEffect)

type Slots = ( ruleButtonPanel :: forall query. H.Slot query Void Int
             , settingsPanel   :: forall query. H.Slot query Void Int
             , proofPanel      :: forall query. H.Slot query Void Int
             )

_proofPanel      = Proxy :: Proxy "proofPanel"
_settingsPanel   = Proxy :: Proxy "settingsPanel"
_ruleButtonPanel = Proxy :: Proxy "ruleButtonPanel"

siteBody :: forall q i output m. MonadEffect m => H.Component q i output m
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
                  [ HH.slot _proofPanel 0 PP.proofPanel 0 identity ]
              , HH.div
                  [ HP.classes [ HH.ClassName "column" ] ]
                  [ HH.slot _ruleButtonPanel 1 RP.ruleButtonPanel 0 identity
                  , HH.slot _settingsPanel 2 SP.settingsPanel 0 identity
                  ]
              ]
          ]
      , SE.siteFooter
      ]