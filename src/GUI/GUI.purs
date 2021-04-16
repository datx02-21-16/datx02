module GUI where

import Data.Maybe (Maybe(..))
import Prelude
import Effect.Class (class MonadEffect)
import GUI.Config.Text as GCT
import GUI.ProofPanel as PP
import GUI.RulesPanel as RP
import GUI.SettingsPanel as SP
import GUI.StaticElements as SE
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Type.Proxy (Proxy(..))

type Slots
  = ( ruleButtonPanel :: forall query output. H.Slot query output Int
    , settingsPanel :: forall query. H.Slot query SP.Output Int
    , proofPanel :: forall query. H.Slot query Void Int
    )

_proofPanel = Proxy :: Proxy "proofPanel"

_settingsPanel = Proxy :: Proxy "settingsPanel"

_ruleButtonPanel = Proxy :: Proxy "ruleButtonPanel"

data Action
  = ActivateModal SP.Modal
  | CloseModals

siteBody :: forall q i output m. MonadEffect m => H.Component q i output m
siteBody =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = Nothing

  render st =
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
                  , HH.slot _settingsPanel 2 SP.settingsPanel 0 ActivateModal
                  ]
              ]
          ]
      , SE.siteFooter
      , modal st
      ]

  modal :: Maybe SP.Modal -> HH.HTML _ _
  modal m = case m of
    Just SP.ManualModal -> manualModal
    Just SP.ShortcutModal -> shortcutModal
    Nothing -> HH.p_ []

  manualModal :: HH.HTML _ _
  manualModal = mkModal "How to use the editor." SE.manualModalBody

  shortcutModal :: HH.HTML _ _
  shortcutModal = mkModal "Syntax shortcuts." SE.shortcutModalBody

  mkModal :: String -> (HH.HTML _ _) -> HH.HTML _ _
  mkModal title modalBody =
    HH.div [ HP.classes [ HH.ClassName "modal", HH.ClassName "is-active" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "modal-background" ]
          , HE.onClick (\_ -> CloseModals)
          ]
          []
      , HH.div [ HP.classes [ HH.ClassName "modal-card" ] ]
          [ HH.header [ HP.classes [ HH.ClassName "modal-card-head" ] ]
              [ HH.p [ HP.classes [ HH.ClassName "modal-card-title" ] ] [ HH.text title ]
              , HH.button
                  [ HP.classes [ HH.ClassName "delete" ]
                  , ARIA.label "close"
                  , HE.onClick (\_ -> CloseModals)
                  ]
                  []
              ]
          , HH.section [ HP.classes [ HH.ClassName "modal-card-body" ] ] [ modalBody ]
          ]
      ]

  handleAction = case _ of
    ActivateModal m -> case m of
      SP.ManualModal -> H.modify_ \st -> Just SP.ManualModal
      SP.ShortcutModal -> H.modify_ \st -> Just SP.ShortcutModal
    CloseModals -> H.modify_ \st -> Nothing
