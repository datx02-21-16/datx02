module GUI.SymbolInput (symbolInput) where

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
import Web.HTML.HTMLInputElement as HIE

import Data.String.Regex (replace)
import Data.Tuple (Tuple(..))
import GUI.Config.Syntax as Syntax

import GUI.Utils (makeRegex)

substituteAll :: String -> String
substituteAll s = foldl substitute s substitutions

substitute :: String -> Tuple String String -> String
substitute str (Tuple from to) = replace (makeRegex from) to str

substitutions :: Array (Tuple String String)
substitutions =
  [ Tuple "an" "∧"
  , Tuple ("""\W+""" <> Syntax.orText <> """\W+""") " v "
  , Tuple ("""\W+""" <> Syntax.impText <> """\W+""") " -> "
  , Tuple ("""\W+""" <> Syntax.notText <> """\W+""") " ¬"
  -- Quantifiers
  , Tuple ("""\W+""" <> Syntax.forAllText <> """\W+""") "∀"
  , Tuple ("""\W+""" <> Syntax.existsText <> """\W+""") "∃"
  , Tuple ("""\W+""" <> Syntax.bottomText <> """\W+""") "⊥"
  ]

type Input = String

data Action
  = OnInput String
  | Receive Input

ref :: H.RefLabel
ref = H.RefLabel "inputRef"

-- | Text field component with abbreviations for logic symbols.
symbolInput :: forall query m. MonadEffect m => String -> H.Component query Input String m
symbolInput placeholder
  = H.mkComponent
    { initialState
      , render
      , eval: H.mkEval H.defaultEval { handleAction = handleAction
                                     , receive = Just <<< Receive }}
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
        ]
    setStr s' = do
        { s, cursor } <- H.get
        let cursor' = cursor + String.length s' - String.length s
        H.modify_ _ { s = s', cursor = cursor' }

        el <- H.getHTMLElementRef ref
        case el >>= HIE.fromHTMLElement of
          Just x -> H.liftEffect do
            HIE.setSelectionStart cursor' x
            HIE.setSelectionEnd cursor' x
          _ -> unsafeCrashWith "not yet rendered"

    handleAction = case _ of
      OnInput s -> do
        el <- H.getHTMLElementRef ref
        case el >>= HIE.fromHTMLElement of
          Just x -> do
            selectionStart <- H.liftEffect $ HIE.selectionStart x
            H.modify_ _ { s = s, cursor = selectionStart }
          _ -> unsafeCrashWith "not yet rendered"

        let s' = substituteAll s
        H.raise s' -- Output the new value
      Receive s' -> setStr s'
