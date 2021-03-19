module GUI.Proof where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Effect.Class (class MonadEffect)
import Effect.Console (logShow)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import GUI.SymbolInput (symbolInput)

type Row
  = { formulaText :: String
    , ruleText :: String
    , ruleArgs :: Array String
    }

type State
  = { premises :: String
    , conclusion :: String
    , rows :: Array Row
    }

data Action
  = UpdateFormula Int String
  | UpdateRule Int

_symbolInput = Proxy :: Proxy "symbolInput"

proof :: forall query input output m. MonadEffect m => H.Component query input output m
proof =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = { premises: ""
                   , conclusion: ""
                   , rows: [
                     { formulaText: "", ruleText: "", ruleArgs: []}
                           ] }

  render st =
      HH.div
          [ HP.classes [ HH.ClassName "panel-block" ] ]
          (mapWithIndex row st.rows)

  row i { formulaText, ruleText }
    = HH.div
      [ HP.classes [ HH.ClassName "columns", HH.ClassName "is-mobile" ] ]
      ( [ HH.div
            [ HP.classes [ HH.ClassName "column" ] ]
            [ HH.h4
                [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
                [ HH.text (show i) ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "is-three-quarters" ] ]
            [ (HH.slot _symbolInput i (symbolInput "Enter formula") formulaText $ UpdateFormula i)
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "is-one-fifth" ] ]
            [ HH.input
                [ HP.value ruleText
                , HP.placeholder "Rule"
                , HP.classes [ HH.ClassName "input", HH.ClassName "is-primary" ]
                , HP.type_ HP.InputText
                , HE.onKeyUp \_ -> UpdateRule 0
                ]
            ]
        ]
      )

  handleAction = case _ of
    UpdateFormula i s -> do
      H.modify_
         \st -> st { rows = unsafePartial $ fromJust $ Array.modifyAt i _ { formulaText = s } st.rows }
    -- UpdateRule _ -> H.modify_ \state -> state { ruleText = substituteAll state.ruleText }
    _ -> unsafeCrashWith "unimpl"
