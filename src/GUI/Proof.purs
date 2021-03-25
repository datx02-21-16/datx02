module GUI.Proof where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Data.Array as Array
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
--import GUI.Panels as P
import GUI.Rules as R

data Rule
  = Rule String
  | Assumption { boxEndIdx :: Int }

instance showRule :: Show Rule where
  show (Rule s) = s
  show (Assumption { boxEndIdx }) = "Assumption (box ends at " <> show boxEndIdx <> ")"

ruleText :: Rule -> String
ruleText r = case r of
  (Rule s) -> s
  (Assumption _) -> "Ass."

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
    }

data Action
  = UpdateFormula Int String
  | UpdateRule Int String
  | NewRowBelow Int
  | TryExit Int

_symbolInput = Proxy :: Proxy "symbolInput"

data Query a
  = Tell R.Rules a

type Slots
  = ( proof :: forall output. H.Slot Query output Int
    , symbolInput :: H.Slot SI.Query SI.Output Int
    )

proof :: forall input output m. MonadEffect m => H.Component Query input output m
proof =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            }
    }
  where
  initialState :: forall m. m -> State
  initialState _ =
    { premises: ""
    , conclusion: ""
    , rows: [ emptyRow ]
    }

  handleQuery :: forall a state action. Query a -> H.HalogenM state action Slots output m (Maybe a)
  handleQuery (Tell command a) = case command of
    R.AndElim1 -> do
      H.liftEffect $ logShow "and elim 1"
      -- When we are here the button click from AndElim1 has been propagated all
      -- the way to the proof component, and we can now update the state accordingly,
      -- inserting new rows etc.
      pure Nothing
    R.AndElim2 -> do
      H.liftEffect $ logShow "and elim 2"
      pure Nothing
    R.AndIntro -> do
      H.liftEffect $ logShow "and introduction"
      pure Nothing
    _ -> pure Nothing

  render st =
    HH.div
      [ HP.classes [ HH.ClassName "proof-rows" ] ]
      ( NonEmpty.head
            $ foldlWithIndex
                ( \i (currentBox@{ elems } :| parentBoxes) proofRow ->
                    let
                      closeBoxesIfPossible = case _ of
                        { endIdx } :| _
                          | endIdx < i -> unsafeCrashWith "Unreachable (box ends outside of parent)"
                        { elems: currentElems, endIdx } :| parent : rest
                          | endIdx == i ->
                            closeBoxesIfPossible
                              $ parent
                                  { elems =
                                    Array.snoc parent.elems
                                      $ HH.div [ HP.classes [ HH.ClassName "proof-box" ] ] currentElems
                                  }
                              :| rest
                        x -> x
                    in
                      closeBoxesIfPossible case proofRow.rule of
                        Rule s -> (currentBox { elems = Array.snoc elems $ row i proofRow }) :| parentBoxes
                        Assumption { boxEndIdx } ->
                          { elems: [ row i proofRow ], endIdx: boxEndIdx }
                            :| currentBox
                            : parentBoxes
                )
                ({ elems: [], endIdx: Array.length st.rows } :| Nil)
                st.rows
        )
        .elems

  row :: Int -> Row -> HH.HTML _ _
  row i { formulaText, rule } =
    HH.div
      [ HP.classes [ HH.ClassName "columns", HH.ClassName "is-mobile", HH.ClassName "proof-row" ] ]
      ( [ HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "is-narrow" ] ]
            [ HH.h4
                [ HP.classes [ HH.ClassName "title", HH.ClassName "row-index" ] ]
                [ HH.text (show (1 + i)) ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "formula-field" ] ]
            [ HH.slot _symbolInput (2 * i) (symbolInput "Enter formula") formulaText
                $ case _ of
                    SI.NewValue s -> UpdateFormula i s
                    SI.EnterPressed -> NewRowBelow i
                    SI.ShiftEnterPressed -> TryExit i
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "is-narrow" ] ]
            [ HH.span
                [ HP.classes [ HH.ClassName "rule-field" ] ]
                [ HH.slot _symbolInput (2 * i + 1) (symbolInput "Rule") (ruleText rule)
                    $ case _ of
                        SI.NewValue s -> UpdateRule i s
                        SI.EnterPressed -> NewRowBelow i
                        SI.ShiftEnterPressed -> TryExit i
                ]
            ]
        ]
      )

  handleAction = case _ of
    UpdateFormula i s ->
      H.modify_ \st ->
        st
          { rows =
            unsafePartial
              $ fromJust
              $ Array.modifyAt i _ { formulaText = s } st.rows
          }
    UpdateRule i s ->
      H.modify_ \st ->
        st
          { rows =
            unsafePartial $ fromJust
              $ Array.modifyAt i _ { rule = ruleFromString s i }
                  st.rows
          }
    NewRowBelow i -> do
      H.modify_ (createBelow i)
      -- Focus the newly added row
      H.tell _symbolInput (2 * (i + 1)) SI.Focus
    TryExit i -> do
      H.modify_ (exitBox i)
      H.tell _symbolInput (2 * (i + 1)) SI.Focus
    _ -> unsafeCrashWith "unimpl"
    where
    createBelow :: Int -> State -> State
    createBelow i st = st { rows = unsafePartial $ fromJust $ Array.insertAt (i + 1) emptyRow $ (incrBoxEnds i) st.rows }

    exitBox :: Int -> State -> State
    exitBox i st = st { rows = unsafePartial $ fromJust $ Array.insertAt (i + 1) emptyRow $ (incrBoxEnds (i + 1)) st.rows }

    incrBoxEnds :: Int -> Array Row -> Array Row
    incrBoxEnds i =
      map \r -> case r of
        row@{ rule: Assumption { boxEndIdx } }
          | i <= boxEndIdx -> row { rule = Assumption { boxEndIdx: boxEndIdx + 1 } }
        x -> x

  getAllEndings :: State -> Array Int
  getAllEndings st = map rToE $ boxes st
    where
    rToE :: Row -> Int
    rToE r = case r.rule of
      Assumption { boxEndIdx } -> boxEndIdx
      _ -> unsafeCrashWith "Row is not a box."

    boxes :: State -> Array Row
    boxes st = Array.filter isBox st.rows

    isBox :: Row -> Boolean
    isBox r = case r.rule of
      Assumption _ -> true
      _ -> false

  ruleFromString :: String -> Int -> Rule
  ruleFromString s rowIdx
    | s == "Ass." || s == "as" = Assumption { boxEndIdx: rowIdx }
    | otherwise = Rule s
