module GUI.ProofPanel where

import Prelude (Void, discard, identity, pure)
import Type.Proxy (Proxy(..))
import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import GUI.Proof as GP

import Data.Maybe (Maybe(..))

type Slots = ( proofPanel :: forall query       . H.Slot query Void Int
             , proof      :: forall output query. H.Slot query output Int)

_proofPanel = Proxy :: Proxy "proofPanel"
_proof      = Proxy :: Proxy "proof"

proofPanel :: forall input output query m. MonadEffect m => H.Component query input output m
proofPanel =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }
  where

  render :: forall state action. state -> H.ComponentHTML action Slots m
  render _ =
    HH.div
      [ HP.classes [ HH.ClassName "panel", HH.ClassName "is-primary" ] ]
      [ HH.p
          [ HP.classes [ HH.ClassName "panel-heading" ] ]
          [ HH.text "Proof" ]
      , HH.div
          [ HP.classes [ HH.ClassName "panel-block" ] ]
          [ HH.slot_ _proof 0 GP.proof { } ]
      ]