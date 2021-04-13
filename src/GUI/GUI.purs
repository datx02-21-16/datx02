module GUI (siteBody) where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import GUI.Config.Text as GCT
import GUI.ProofPanel as PP
import GUI.RulesPanel as RP
import GUI.SettingsPanel as SP
import GUI.StaticElements as SE
import GUI.RawHTML as RawHTML
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

_proofPanel = Proxy :: Proxy "proofPanel"

_settingsPanel = Proxy :: Proxy "settingsPanel"

_ruleButtonPanel = Proxy :: Proxy "ruleButtonPanel"

type Slots
  = ( proofPanel :: PP.Slot Unit
    , ruleButtonPanel :: RP.Slot Unit
    , settingsPanel :: SP.Slot Unit
    , rawHTML :: RawHTML.Slot Int
    )

type State
  = Maybe SP.Modal

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

  render :: State -> H.ComponentHTML Action Slots m
  render st =
    HH.div
      [ HP.classes
          [ HH.ClassName "container" ]
      ]
      ( [ SE.siteHeader GCT.editorName GCT.editorSlogan
        , HH.section
            [ HP.classes [ HH.ClassName "section" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "columns" ] ]
                [ HH.div
                    [ HP.classes [ HH.ClassName "column", HH.ClassName "is-three-quarters" ] ]
                    [ HH.slot_ _proofPanel unit PP.proofPanel 0 ]
                , HH.div
                    [ HP.classes [ HH.ClassName "column" ] ]
                    [ HH.slot_ _ruleButtonPanel unit RP.ruleButtonPanel 0
                    , HH.slot _settingsPanel unit SP.settingsPanel 0 ActivateModal
                    ]
                ]
            ]
        , SE.siteFooter
        ]
          <> ( case st of
                Just m -> [ modal m ]
                Nothing -> []
            )
      )

  modal :: SP.Modal -> HH.HTML _ _
  modal m = case m of
    SP.ManualModal -> manualModal
    SP.ShortcutModal -> shortcutModal

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

  manualModal :: Boolean -> HH.HTML _ _
  manualModal isActive = mkModal "How to use the editor." manualBody isActive
    where
    manualBody = HH.text "Show the manual"

  shortcutModal :: Boolean -> HH.HTML _ _
  shortcutModal isActive = mkModal "Syntax shortcuts." shortcutBody isActive
    where
    shortcutBody = HH.text "Show the shortcuts"

  mkModal :: String -> (HH.HTML _ _) -> Boolean -> HH.HTML _ _
  mkModal title modalBody isActive =
    HH.div [ HP.classes $ [ HH.ClassName "modal" ] <> if isActive then [ HH.ClassName "is-active" ] else [] ]
      [ HH.div [ HP.classes [ HH.ClassName "modal-background" ] ] []
      , HH.div [ HP.classes [ HH.ClassName "modal-card" ] ]
          [ HH.header [ HP.classes [ HH.ClassName "modal-card-head" ] ]
              [ HH.p [ HP.classes [ HH.ClassName "modal-card-title" ] ] [ HH.text title ]
              , HH.button [ HP.classes [ HH.ClassName "delete" ], ARIA.label "close" ] []
              ]
          , HH.section [ HP.classes [ HH.ClassName "modal-card-body" ] ] [ modalBody ]
          --, HH.footer [ HP.classes [ HH.ClassName "modal-card-foot" ] ] [ HH.text "Manual footer" ]
          ]
      ]

  handleAction = case _ of
    ActivateModal m -> H.put (Just m)
    CloseModals -> H.put Nothing
