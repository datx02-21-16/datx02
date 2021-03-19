module GUI.SymbolInput (Output(..), Query(..), symbolInput) where

import Prelude
import Data.Array (foldl)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import Data.String as String
import Effect.Class (class MonadEffect)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

import Data.String.Regex (replace)
import Data.Tuple (Tuple(..))

import GUI.Utils (makeRegex)

substitute :: String -> String
substitute = foldl (<<<) identity
             $ (\(Tuple from to) -> replace (makeRegex from) to) <$> ss
  where
    ss = [ Tuple "an" "∧"
         , Tuple "or" "∨"
         , Tuple "->" "→"
         , Tuple "no" "¬"
           -- Quantifiers
         , Tuple "f[ao]" "∀"
         , Tuple "ex|te" "∃"
         , Tuple "bo" "⊥"
         ]

data Query a = Focus a

type Input = String

data Output = NewValue String
            | EnterPressed

data Action
  = OnInput String
  | Receive Input
  | KeyDown KeyboardEvent

ref :: H.RefLabel
ref = H.RefLabel "inputRef"

-- | Text field component with abbreviations for logic symbols.
symbolInput :: forall m. MonadEffect m => String -> H.Component Query Input Output m
symbolInput placeholder
  = H.mkComponent
    { initialState
      , render
      , eval: H.mkEval H.defaultEval { handleAction = handleAction
                                     , receive = Just <<< Receive
                                     , handleQuery = handleQuery }}
  where
    initialState s = { s, cursor: String.length s }
    render st
      = HH.input
        [ HP.value st.s
        , HP.placeholder placeholder
        , HP.classes [ HH.ClassName "input", HH.ClassName "is-primary" ]
        , HP.type_ HP.InputText
        , HP.ref ref
        , HE.onValueInput OnInput
        , HE.onKeyDown KeyDown
        ]
    setStr s' = do
        { s, cursor } <- H.get
        -- Note: Assumes all edits happen before cursor
        let cursor' = cursor + String.length s' - String.length s
        -- Update the value: This will prompt a redraw
        H.modify_ _ { s = s', cursor = cursor' }

        -- Reset the cursor position
        el <- H.getHTMLElementRef ref
        case el >>= HTMLInputElement.fromHTMLElement of
          Just x -> H.liftEffect do
            HTMLInputElement.setSelectionStart cursor' x
            HTMLInputElement.setSelectionEnd cursor' x
          _ -> unsafeCrashWith "not yet rendered"

    handleAction = case _ of
      OnInput s -> do
        el <- H.getHTMLElementRef ref
        case el >>= HTMLInputElement.fromHTMLElement of
          Just x -> do
            selectionStart <- H.liftEffect $ HTMLInputElement.selectionStart x
            H.modify_ _ { s = s, cursor = selectionStart }
          _ -> unsafeCrashWith "not yet rendered"

        let s' = substitute s
        H.raise $ NewValue s' -- Output the new value
      Receive s' -> setStr s'
      KeyDown ev -> when (KeyboardEvent.key ev == "Enter")
                    $ H.raise EnterPressed

    handleQuery :: forall action a. Query a -> H.HalogenM _ action () _ m (Maybe a)
    handleQuery = case _ of
      Focus a -> do
        el <- H.getHTMLElementRef ref
        case el >>= HTMLInputElement.fromHTMLElement of
          Just x -> H.liftEffect $ HTMLElement.focus (HTMLInputElement.toHTMLElement x)
          _ -> unsafeCrashWith "not yet rendered"
        pure $ Just a
