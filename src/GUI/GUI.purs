module GUI where

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
  = ( ruleButtonPanel :: H.Slot RP.Query RP.Output Int
    , settingsPanel :: forall query. H.Slot query SP.Output Int
    , proofPanel :: H.Slot PP.Query Void Int
    )

_proofPanel = Proxy :: Proxy "proofPanel"

_settingsPanel = Proxy :: Proxy "settingsPanel"

_ruleButtonPanel = Proxy :: Proxy "ruleButtonPanel"

data Action
  = HandleButton RP.Output
  | ActivateModal SP.Modal
  | CloseModals

siteBody :: forall q i output m. MonadEffect m => H.Component q i output m
siteBody =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = { showManualModal: false, showShortcutModal: false }

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
                  [ HH.slot _ruleButtonPanel 1 RP.ruleButtonPanel 0 HandleButton
                  , HH.slot _settingsPanel 2 SP.settingsPanel 0 ActivateModal
                  ]
              ]
          ]
      , SE.siteFooter
      , manualModal st.showManualModal
      , shortcutModal st.showShortcutModal
      ]

  manualModal :: Boolean -> HH.HTML _ _
  manualModal isActive = mkModal "How to use the editor." manualBody isActive
    where
    manualBody = HH.text lorem

  shortcutModal :: Boolean -> HH.HTML _ _
  shortcutModal isActive = mkModal "Syntax shortcuts." shortcutBody isActive
    where
    shortcutBody = HH.text lorem

  mkModal :: String -> (HH.HTML _ _) -> Boolean -> HH.HTML _ _
  mkModal title modalBody isActive =
    HH.div [ HP.classes $ [ HH.ClassName "modal" ] <> if isActive then [ HH.ClassName "is-active" ] else [] ]
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
    HandleButton output -> do
      H.tell _proofPanel 0 (PP.Tell output)
      H.modify_ \st -> st
    ActivateModal m -> case m of
      SP.ManualModal -> H.modify_ \st -> st { showManualModal = true }
      SP.ShortcutModal -> H.modify_ \st -> st { showShortcutModal = true }
    CloseModals -> H.modify_ \st -> st { showManualModal = false, showShortcutModal = false }

-- Placeholder
lorem :: String
lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce lobortis nulla a eros ullamcorper, vitae tincidunt tortor auctor. Quisque quis sapien lobortis, rutrum diam quis, porttitor urna. Donec tristique ut quam sit amet luctus. In finibus est feugiat maximus blandit. Nunc vel accumsan neque, sed tristique justo. Vestibulum sit amet vulputate felis, vitae malesuada dolor. Proin odio lacus, iaculis sed luctus et, pellentesque eget tortor. Curabitur maximus lorem massa. Donec ornare pretium justo eu malesuada. Aenean sed libero molestie, consequat justo eu, dapibus nisi. Cras vestibulum sapien at risus luctus, id fermentum massa mattis. Aliquam nec nulla dui. Praesent laoreet enim suscipit lorem facilisis, volutpat laoreet turpis aliquam. Vivamus cursus efficitur diam quis cursus. Integer tempus convallis metus vitae tincidunt. Vestibulum maximus maximus euismod. Fusce imperdiet et tellus luctus pellentesque. In eu egestas velit. Phasellus luctus lectus vitae justo venenatis accumsan. Suspendisse dapibus quam vitae pharetra gravida. Vivamus sit amet elementum tellus, sit amet maximus sem. Etiam cursus pellentesque elit, sit amet volutpat tellus maximus quis. Sed molestie tortor neque, nec sollicitudin turpis luctus sit amet. Mauris eu massa at nisl condimentum congue. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec tincidunt dignissim mauris, facilisis vehicula metus mattis id. Donec quis risus ipsum. Praesent faucibus faucibus ullamcorper. Cras eu fringilla magna, ac sodales augue. Aliquam aliquet congue velit, vitae vulputate nunc elementum ut. Cras ultricies consectetur commodo. Proin rhoncus quam sed leo blandit, et feugiat justo iaculis. Nam odio odio, tincidunt id augue vel, vulputate gravida purus. Sed semper risus nec nisi aliquet iaculis quis non augue. Maecenas quis turpis ut mauris ultrices facilisis. In quis est accumsan erat consequat placerat vel pretium enim. Nunc accumsan, neque eget dictum malesuada, ante metus rhoncus lacus, ac volutpat nunc magna rutrum felis. Donec quis turpis at tellus mattis porta a eu dolor. Quisque venenatis lacus a ligula lacinia dignissim. Aliquam placerat mi a ullamcorper euismod. Nullam sit amet nisl neque. Phasellus nec lobortis nunc. Quisque feugiat volutpat aliquet. Nunc ut pulvinar velit. Praesent sed tellus erat. Nulla volutpat ipsum nec ipsum ornare, id condimentum nisl varius. Curabitur nec purus ac leo rutrum fringilla. Donec eu mi ut lectus dapibus sodales. Phasellus hendrerit facilisis ornare. Donec mi augue, volutpat quis semper non, aliquet a elit. Phasellus commodo sagittis velit, a bibendum quam suscipit eu. Suspendisse potenti. Nullam posuere neque euismod, vestibulum purus vitae, fermentum orci. Curabitur elementum dui eu quam consequat maximus. Aliquam quis erat tincidunt quam ornare tincidunt. Integer viverra efficitur suscipit. Sed volutpat lectus vitae ultricies consequat. Cras ut porttitor magna. Vestibulum tortor augue, mollis a tempus nec, suscipit vitae lectus. Aliquam a bibendum turpis. Pellentesque id tempor orci. Duis elit massa, accumsan eu turpis sed, consectetur molestie ipsum. Vivamus eu commodo lectus. Donec facilisis mi id lectus auctor tempus. Curabitur nec mauris nec lorem varius consequat. Fusce tristique ligula vel lectus fermentum, nec laoreet est dapibus. In ultricies mauris nibh, nec ultrices tellus viverra maximus. Ut a mi nec mi scelerisque facilisis sit amet eget elit. Curabitur semper nulla."
