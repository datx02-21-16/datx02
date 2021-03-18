module GUI.Panels where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Array as Array
import Partial.Unsafe (unsafePartial)
import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import GUI.Config.Text as GCT
import GUI.Proof as GP

type Panel
  = H.Component

type Slots = ( proof :: forall query. H.Slot query Void Int )

_proof = Proxy :: Proxy "proof"

proofPanel :: forall query input output m. MonadEffect m => H.Component query input output m
proofPanel =
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
          [ HH.text "Proof" ]
      , HH.div
          [ HP.classes [ HH.ClassName "panel-block" ] ]
          [ HH.slot_ _proof 1 GP.proof { } ]
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
