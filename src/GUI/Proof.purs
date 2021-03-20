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
import Halogen.HTML.Properties as HP

import GUI.SymbolInput as SI
import GUI.SymbolInput (symbolInput)

type Row
  = { formulaText :: String
    , ruleText :: String
    , ruleArgs :: Array String
    }

emptyRow :: Row
emptyRow = { formulaText: "", ruleText: "", ruleArgs: [] }

type State
  = { premises :: String
    , conclusion :: String
    , rows :: Array Row
    }

data Action
  = UpdateFormula Int String
  | UpdateRule Int String
  | NewRowBelow Int

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
                   , rows: [ emptyRow ] }

  render st =
      HH.div
          [ HP.classes [ HH.ClassName "proof-box" ] ]
          [ row 0 { formulaText: "A", ruleText: "" }
          , HH.div [ HP.classes [ HH.ClassName "proof-box" ] ]
            [ row 1 { formulaText: "B", ruleText: "" }
            , row 2 { formulaText: "A", ruleText: "" } ]
          , row 3 { formulaText: "A", ruleText: "" }
          ]

  _render st =
      HH.div
          [ HP.classes [] ]
          (mapWithIndex row st.rows)

  row i { formulaText, ruleText }
    = HH.div
      [ HP.classes [ HH.ClassName "columns", HH.ClassName "is-mobile" ] ]
      ( [ HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "is-narrow" ] ]
            [ HH.h4
                [ HP.classes [ HH.ClassName "title" ] ]
                [ HH.text (show (1+i)) ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "column" ] ]
            [ HH.slot _symbolInput (2*i) (symbolInput "Enter formula") formulaText $ case _ of
                 SI.NewValue s -> UpdateFormula i s
                 SI.EnterPressed -> NewRowBelow i
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "is-narrow" ] ]
            [ HH.span
              [ HP.classes [ HH.ClassName "rule-field" ] ]
              [ HH.slot _symbolInput (2*i+1) (symbolInput "Rule") ruleText $ case _ of
                 SI.NewValue s -> UpdateRule i s
                 SI.EnterPressed -> NewRowBelow i ]
            ]
        ]
      )

  handleAction = case _ of
    UpdateFormula i s ->
      H.modify_
         \st -> st { rows = unsafePartial $ fromJust $ Array.modifyAt i _ { formulaText = s } st.rows }
    UpdateRule i s ->
      H.modify_
         \st -> st { rows = unsafePartial $ fromJust $ Array.modifyAt i _ { ruleText = s } st.rows }
    NewRowBelow i -> do
      H.modify_ \st -> st { rows = unsafePartial $ fromJust $ Array.insertAt
                                   (i+1) emptyRow st.rows
                          }
      -- Focus the newly added row
      H.tell _symbolInput (2*(i+1)) SI.Focus

    _ -> unsafeCrashWith "unimpl"
