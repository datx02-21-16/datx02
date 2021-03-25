module GUI.RulesPanel where

import Data.Maybe

import Effect.Class (class MonadEffect)
import GUI.Proof as GP
import GUI.Rules as R
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, discard, identity, pure, ($))
import Type.Proxy (Proxy(..))

type Slots = ( proofPanel ::                H.Slot Query Void Int
             , proof      :: forall output. H.Slot GP.Query output Int)

_proofPanel = Proxy :: Proxy "proofPanel"
_proof      = Proxy :: Proxy "proof"

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

type Action = R.Rules
type Output = R.Rules

ruleButtonPanel :: forall query m . MonadEffect m => H.Component query Int Output m
ruleButtonPanel =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction}
    }
  where

  render _ =
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
      , HH.button
          [ HP.classes [ HH.ClassName "button" ]
          , HP.type_ HP.ButtonSubmit
          , HE.onClick (\_ -> R.OrIntro)
          ]
          [ HH.text "∨i" ]
      , HH.button
          [ HP.classes [ HH.ClassName "button" ]
          , HP.type_ HP.ButtonSubmit
          , HE.onClick (\_ -> R.NotIntro)
          ]
          [ HH.text "¬i" ]
      , HH.button 
          [ HP.classes [ HH.ClassName "button"]
          , HP.type_ HP.ButtonSubmit
          , HE.onClick (\_ -> R.NotElim)
          ]
          [ HH.text "¬e"] 
      ]

    
  handleAction :: forall state. Action -> H.HalogenM state Action () Output m Unit
  handleAction action = H.raise action