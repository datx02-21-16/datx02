module GUI.ProofPanel (Slot, proofPanel) where

import Prelude
import Type.Proxy (Proxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import GUI.Proof as GP

type Slot id
  = forall query. H.Slot query Void id

type Slots
  = ( proof :: forall output query. H.Slot query output Unit )

_proof = Proxy :: Proxy "proof"

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
          [ HH.slot_ _proof unit GP.proof {} ]
      ]
