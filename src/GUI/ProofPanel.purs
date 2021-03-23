module GUI.ProofPanel where

import Prelude (Void, discard, identity, pure)
import Type.Proxy (Proxy(..))
import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import GUI.Proof as GP
import GUI.Rules as R

import Data.Maybe (Maybe(..))

type Command = R.Rules

type Slots = ( proofPanel ::                H.Slot Query Void Int
             , proof      :: forall output. H.Slot GP.Query output Int)

_proofPanel = Proxy :: Proxy "proofPanel"
_proof      = Proxy :: Proxy "proof"

data Query a = Tell Command a

proofPanel :: forall input output m. MonadEffect m => H.Component Query input output m
proofPanel =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval { handleQuery = handleQuery}
    }
  where
  
  handleQuery :: forall a state action. Query a -> H.HalogenM state action Slots output m (Maybe a)
  handleQuery (Tell command a) = do
    H.tell _proof 0 (GP.Tell command)
    pure (Just a)

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