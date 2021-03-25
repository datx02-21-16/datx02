module GUI.Proof (Query(..), proof) where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Data.Array as Array
import Data.Array ((!!))
import Data.FunctorWithIndex (mapWithIndex)
import Effect.Class (class MonadEffect)
import Effect.Console (logShow)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(Nil), (:))
import Data.NonEmpty as NonEmpty
import Data.NonEmpty ((:|))
import Data.MediaType (MediaType(MediaType))
import Data.Int as Int
import Data.String.Common (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Web.Event.Event as Event
import Web.HTML.Event.DragEvent as DragEvent
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DataTransfer as DataTransfer
import Util (moveWithin)
import GUI.SymbolInput as SI
import GUI.SymbolInput (symbolInput)
import GUI.Rules as R

-- For GUI proof state we use a representation that is easy to modify,
-- i.e. has a single contiguous array of all rows. When rendering or
-- validating we map this to a tree with subproof nodes.
data Rule
  = Rule String
  | Premise
  | Assumption
    { boxEndIdx :: Int -- Inclusive end of box
    }

derive instance eqRule :: Eq Rule

instance showRule :: Show Rule where
  show (Rule s) = s
  show Premise = "Premise"
  show (Assumption { boxEndIdx }) = "Assumption (box ends at " <> show boxEndIdx <> ")"

ruleText :: Rule -> String
ruleText (Rule s) = s

ruleText Premise = "Premise"

ruleText (Assumption _) = "Ass."

type ProofRow
  = { formulaText :: String
    , rule :: Rule
    , ruleArgs :: Array String
    }

-- | A newly added row.
emptyRow :: ProofRow
emptyRow = { formulaText: "", rule: Rule "", ruleArgs: [] }

-- | Only stores endpoints of boxes since assumptions naturally define start points.
type State
  = { conclusion :: String
    , rows :: Array ProofRow
    , draggingOver :: Maybe Int
    }

data Action
  = UpdateFormula Int String
  | UpdateRule Int String
  | NewRowBelow Int
  | DragStart Int DragEvent
  | DragOver Int DragEvent
  | DragEnter Int DragEvent
  | DragLeave Int DragEvent
  | DragEnd Int DragEvent
  | Drop Int DragEvent
  | UpdateConclusion String

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

initialState :: forall input. input -> State
initialState _ =
  { conclusion: ""
  , rows: [ emptyRow ]
  , draggingOver: Nothing
  }

handleQuery :: forall a state action output m. MonadEffect m => Query a -> H.HalogenM state action Slots output m (Maybe a)
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

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render st =
  HH.div [ HP.classes [ HH.ClassName "proof" ] ]
    [ proofHeader, proofRows ]
  where
  proofHeader =
    HH.div
      [ HP.classes [ HH.ClassName "proof-header" ] ]
      [ HH.span [] [ HH.text premises, HH.text " ⊢ " ]
      , HH.slot _symbolInput (-1) (symbolInput "Conclusion") st.conclusion
          $ case _ of
              SI.NewValue s -> UpdateConclusion s
              _ -> unsafeCrashWith "todo"
      ]

  premises =
    joinWith ", "
      $ _.formulaText
      <$> Array.takeWhile ((_ == Rule "Premise") <<< _.rule) st.rows

  proofRows :: HH.HTML _ _
  proofRows =
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
                        Assumption { boxEndIdx } ->
                          { elems: [ row i proofRow ], endIdx: boxEndIdx }
                            :| currentBox
                            : parentBoxes
                        _ -> (currentBox { elems = Array.snoc elems $ row i proofRow }) :| parentBoxes
                )
                ({ elems: [], endIdx: Array.length st.rows } :| Nil)
                st.rows
        )
        .elems

  row :: Int -> ProofRow -> HH.HTML _ _
  row i { formulaText, rule } =
    HH.div
      [ HP.classes
          ( [ HH.ClassName "columns", HH.ClassName "is-mobile", HH.ClassName "proof-row" ]
              <> maybe []
                  ( \j ->
                      if i == j then
                        [ HH.ClassName "dragged-over" ]
                      else
                        []
                  )
                  st.draggingOver
          )
      , HP.draggable true
      , HE.onDragStart $ DragStart i
      , HE.onDragOver $ DragOver i
      , HE.onDragEnter $ DragEnter i
      , HE.onDragLeave $ DragLeave i
      , HE.onDrop $ Drop i
      , HE.onDragEnd $ DragEnd i
      ]
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
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "column", HH.ClassName "is-narrow" ] ]
            [ HH.span
                [ HP.classes [ HH.ClassName "rule-field" ] ]
                [ HH.slot _symbolInput (2 * i + 1) (symbolInput "Rule") (ruleText rule)
                    $ case _ of
                        SI.NewValue s -> UpdateRule i s
                        SI.EnterPressed -> NewRowBelow i
                ]
            ]
        ]
      )

-- | The media type for the index of a proof row as a string.
rowMediaType :: MediaType
rowMediaType = MediaType "application/x.row"

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  UpdateFormula i s -> H.modify_ \st -> st { rows = unsafePartial $ fromJust $ Array.modifyAt i _ { formulaText = s } st.rows }
  UpdateRule i s ->
    H.modify_ \st ->
      st
        { rows =
          unsafePartial $ fromJust
            $ Array.modifyAt i _ { rule = ruleFromString s i }
                st.rows
        }
  NewRowBelow i -> do
    H.modify_ \st ->
      let
        incrBoxEnds =
          mapWithIndex \j -> case _ of
            row@{ rule: Assumption { boxEndIdx } }
              | i <= boxEndIdx -> row { rule = Assumption { boxEndIdx: boxEndIdx + 1 } }
            x -> x
      in
        st
          { rows =
            unsafePartial $ fromJust $ Array.insertAt (i + 1) emptyRow
              $ incrBoxEnds st.rows
          }
    -- Focus the newly added row
    H.tell _symbolInput (2 * (i + 1)) SI.Focus
  DragStart i ev -> do
    H.liftEffect $ DataTransfer.setData rowMediaType (show i)
      $ DragEvent.dataTransfer ev
  DragOver i ev -> do
    validDropZone <- isValidDropZone i ev
    when validDropZone do
      H.modify_ \st -> st { draggingOver = Just i }
      H.liftEffect $ Event.preventDefault $ DragEvent.toEvent ev
  DragEnter i ev -> do
    validDropZone <- isValidDropZone i ev
    when validDropZone do
      H.liftEffect $ DataTransfer.setDropEffect DataTransfer.Move $ DragEvent.dataTransfer ev
      H.modify_ \st -> st { draggingOver = Just i }
  DragLeave i ev -> do
    draggingOver <- H.gets _.draggingOver
    when (draggingOver /= Just i) do
      H.liftEffect $ DataTransfer.setDropEffect DataTransfer.None $ DragEvent.dataTransfer ev
      H.modify_ \st -> st { draggingOver = Nothing }
  DragEnd i ev -> H.modify_ \st -> st { draggingOver = Nothing }
  Drop i ev -> do
    H.liftEffect $ Event.preventDefault $ DragEvent.toEvent ev
    H.modify_ \st -> st { draggingOver = Nothing }
    { start, end } <- draggedRows ev
    H.modify_ \st ->
      let
        target = i + 1

        newStart = target - if start < target then end - start else 0

        updateBoxes =
          mapWithIndex \j -> case _ of
            row@{ rule: Assumption { boxEndIdx } }
              -- Moved box
              | start <= j, j < end -> row { rule = Assumption { boxEndIdx: boxEndIdx + (newStart - start) } }
              -- Move before/inside box
              | i <= boxEndIdx, boxEndIdx < start -> row { rule = Assumption { boxEndIdx: boxEndIdx + (end - start) } }
              -- Move after box
              | start <= boxEndIdx, boxEndIdx < i -> row { rule = Assumption { boxEndIdx: boxEndIdx - (end - start) } }
            x -> x

        rows' = moveWithin target start end $ updateBoxes st.rows
      in
        st { rows = rows' }
  UpdateConclusion s -> H.modify_ \st -> st { conclusion = s }
  where
  -- | Inclusive-exclusive interval of the rows that are currently being dragged.
  draggedRows :: DragEvent -> H.HalogenM _ _ _ _ _ { start :: Int, end :: Int }
  draggedRows ev = do
    start <-
      (\s -> unsafePartial $ fromJust $ Int.fromString s)
        <$> (H.liftEffect $ DataTransfer.getData rowMediaType $ DragEvent.dataTransfer ev)
    rows <- H.gets _.rows
    let
      startRow = unsafePartial $ fromJust $ rows !! start
    let
      end = case startRow.rule of
        Assumption { boxEndIdx } -> boxEndIdx + 1
        _ -> start + 1
    pure { start, end }

  isValidDropZone i ev = (\{ start, end } -> not (start <= i && i < end)) <$> draggedRows ev

ruleFromString :: String -> Int -> Rule
ruleFromString s rowIdx
  | s == "Ass.", s == "as" = Assumption { boxEndIdx: rowIdx }
  | s == "pr", s == "Premise" = Premise
  | otherwise = Rule s
