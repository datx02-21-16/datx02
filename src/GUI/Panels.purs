module GUI.Panels where

import Prelude
import Type.Proxy (Proxy(..))
import Effect.Class (class MonadEffect)
import Effect.Console (log)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS

import GUI.Config.Text as GCT
import GUI.Proof as GP
import GUI.Rules as R

import Data.Maybe

type Panel
  = H.Component

-- why do we need to specify Query type for everyone? What exactly are slots?
type Slots = ( proofPanel :: H.Slot Query Void Int
             , proof :: forall output. H.Slot GP.Query output Int)

_proofPanel = Proxy :: Proxy "proofPanel"
_proof = Proxy :: Proxy "proof"

data Query a = Tell Output a

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
    -- do the stuff
    H.tell _proof 0 (GP.Tell command)
    pure (Just a)

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

data State = St Int
type Action = R.Rules --AndElim1 | AndElim2 | AndIntro | DoNothing
type Output = R.Rules
--data Output = Clicked


ruleButtonPanel :: forall query m . MonadEffect m => H.Component query Int Output m
ruleButtonPanel =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction}
    }
  where
  initialState i = St i

  render (St st) =
    HH.div
      [ HP.classes [ HH.ClassName "panel", HH.ClassName "is-primary" ] ]
      [ HH.p
          [ HP.classes [ HH.ClassName "panel-heading" ] ]
          [ HH.text "Rules" ]
      , HH.button
          [ HP.classes [ HH.ClassName "button" ]
          , HP.type_ HP.ButtonSubmit
          , HE.onClick (\_ -> R.AndElim1)
          ]
          [ HH.text "∧e1" ]
      , HH.button
          [ HP.classes [ HH.ClassName "button"]
          , HP.type_ HP.ButtonSubmit
          , HE.onClick $ \_ -> R.AndElim2
          ]
          [ HH.text "∧e2" ]
      , HH.button
          [ HP.classes [ HH.ClassName "button"]
          , HP.type_ HP.ButtonSubmit
          , HE.onClick (\_ -> R.AndIntro)
          ]
          [ HH.text "∧i" ]
      ]
    
  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
      R.AndElim1 -> do
          H.liftEffect $ log "and elim 1"
          H.modify_ \st -> st
          H.raise R.AndElim1 --Clicked
      R.AndElim2 -> H.modify_ \st -> st --GP.handleAction UpdateRule
      R.AndIntro -> H.modify_ \st -> st
      _ -> H.modify_ \st -> st

settingsPanel :: forall t11 t12 t31 t34. Panel t34 t31 t12 t11
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
          [ HH.text GCT.panelNotImplemented ]
      ]

