module GUI.Proof where

import Prelude
import Data.Maybe (Maybe(..))
import GUI.Utils (substituteAll)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type LineData
  = { number :: Int
    , formulaText :: String
    , ruleText :: String
    , ruleArgs :: Array String
    }

type Proof
  = { premises :: String
    , conclusion :: String
    , lines :: Array LineData
    }

type ProofLine
  = H.Component HH.HTML

data Action
  = UpdateFormula
  | UpdateRule

newProof :: String -> String -> Array LineData -> Proof
newProof prems conc lines =
  { premises: prems
  , conclusion: conc
  , lines: lines
  }

emptyLine :: Int -> LineData
emptyLine n =
  { number: n
  , formulaText: ""
  , ruleText: ""
  , ruleArgs: []
  }

proofLine :: forall t25 t5 t6. Int -> ProofLine t25 Int t6 t5
proofLine n =
  H.mkComponent
    { initialState: \_ -> emptyLine n
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  render state =
    HH.div
      [ HP.classes [ HH.ClassName "columns", HH.ClassName "is-mobile" ] ]
      ( [ HH.div
            [ HP.classes [ HH.ClassName "column" ] ]
            [ HH.h4
                [ HP.classes [ HH.ClassName "title", HH.ClassName "is-4" ] ]
                [ HH.text (show state.number) ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "is-three-quarters" ] ]
            [ HH.input
                [ HP.value state.formulaText
                , HP.placeholder "Enter formula"
                , HP.classes [ HH.ClassName "input", HH.ClassName "is-primary" ]
                , HP.type_ HP.InputText
                , HE.onKeyUp \_ -> Just UpdateFormula
                ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "is-one-fifth" ] ]
            [ HH.input
                [ HP.value state.ruleText
                , HP.placeholder "Rule"
                , HP.classes [ HH.ClassName "input", HH.ClassName "is-primary" ]
                , HP.type_ HP.InputText
                , HE.onKeyUp \_ -> Just UpdateRule
                ]
            ]
        ]
      )

  handleAction = case _ of
    UpdateFormula -> H.modify_ \state -> state { formulaText = substituteAll state.formulaText }
    UpdateRule -> H.modify_ \state -> state { ruleText = substituteAll state.ruleText }
