-- | This module defines a Halogen component that renders a given
-- | static string of raw HTML.
module GUI.RawHTML (Slot, component) where

import Prelude
import Data.Maybe (Maybe(Just), fromMaybe')
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Web.DOM.Element (Element)
import Web.HTML.HTMLElement as HTMLElement
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)

type Slot id
  = forall query output. H.Slot query output id

type Input
  = String

type State
  = String

data Action
  = Initialize

refLabel :: H.RefLabel
refLabel = H.RefLabel "raw-root"

component :: forall query output m. MonadEffect m => H.Component query Input output m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _ = HH.div [ HP.classes [ H.ClassName "content" ], HP.ref refLabel ] []

foreign import setInnerHTML :: String -> Element -> Effect Unit

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction Initialize = do
  elem <-
    HTMLElement.toElement <<< fromMaybe' (\_ -> unsafeCrashWith "Missing ref")
      <$> H.getHTMLElementRef refLabel
  s <- H.get
  H.liftEffect $ setInnerHTML s elem
