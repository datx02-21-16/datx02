module GUI.Proof where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Effect.Class (class MonadEffect)
import Effect.Console (logShow)
import Data.Set as Set
import Data.Set (Set)
import Data.FoldableWithIndex (foldlWithIndex)

import Data.List as List
import Data.List (List(Nil), (:))
import Data.NonEmpty as NonEmpty
import Data.NonEmpty (NonEmpty, (:|))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import GUI.SymbolInput as SI
import GUI.SymbolInput (symbolInput)

data Rule = Rule String
          | Assumption { boxEndIdx :: Int }

ruleText :: Rule -> String
ruleText (Rule s) = s
ruleText (Assumption _) = "Ass."

type Row
  = { formulaText :: String
    , rule :: Rule
    , ruleArgs :: Array String
    }

emptyRow :: Row
emptyRow = { formulaText: "", rule: Rule "", ruleArgs: [] }

-- | Only stores endpoints of boxes since assumptions naturally define start points.
type State
  = { premises :: String
    , conclusion :: String
    , rows :: Array Row
    , boxEnds :: Set Int
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
          [ HP.classes [ HH.ClassName "proof-rows" ] ]
          (NonEmpty.head $ foldlWithIndex
             (\i (currentBox@{ elems }:|parentBoxes) proofRow
              -> let
                closeBoxesIfPossible = case _ of
                  {elems: currentElems, endIdx}:|parent:rest
                    | endIdx == i -> parent { elems = Array.snoc parent.elems
                                                      $ HH.div [ HP.classes [ HH.ClassName "proof-box" ] ]
                                                      currentElems }:|rest
                  x -> x

                next = case proofRow.rule of
                       Rule s -> (currentBox { elems = Array.snoc elems $ row i proofRow }):|parentBoxes
                       Assumption { boxEndIdx }
                         -> { elems: [row i proofRow], endIdx: boxEndIdx }
                            :|currentBox:parentBoxes
                 in closeBoxesIfPossible next
             )
             ({ elems: [], endIdx: Array.length st.rows }:|Nil)
             st.rows).elems

  row :: Int -> Row -> HH.HTML _ _
  row i { formulaText, rule }
    = HH.div
      [ HP.classes [ HH.ClassName "columns", HH.ClassName "is-mobile", HH.ClassName "proof-row" ] ]
      ( [ HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "is-narrow" ] ]
            [ HH.h4
                [ HP.classes [ HH.ClassName "title", HH.ClassName "row-index" ] ]
                [ HH.text (show (1+i)) ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "formula-field" ] ]
            [ HH.slot _symbolInput (2*i) (symbolInput "Enter formula") formulaText $ case _ of
                 SI.NewValue s -> UpdateFormula i s
                 SI.EnterPressed -> NewRowBelow i
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "is-narrow" ] ]
            [ HH.span
              [ HP.classes [ HH.ClassName "rule-field" ] ]
              [ HH.slot _symbolInput (2*i+1) (symbolInput "Rule") (ruleText rule) $ case _ of
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
      \st -> st { rows = unsafePartial $ fromJust
                         $ Array.modifyAt i _ { rule = ruleFromString s i }
                         st.rows }
    NewRowBelow i -> do
      H.modify_ \st -> st { rows = unsafePartial $ fromJust $ Array.insertAt
                                   (i+1) emptyRow st.rows
                          }
      -- Focus the newly added row
      H.tell _symbolInput (2*(i+1)) SI.Focus

    _ -> unsafeCrashWith "unimpl"

  ruleFromString :: String -> Int -> Rule
  ruleFromString s rowIdx
    | s == "Ass." || s == "as" = Assumption { boxEndIdx: rowIdx }
    | otherwise = Rule s
