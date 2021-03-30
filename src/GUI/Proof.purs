module GUI.Proof (Query(..), proof) where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Either (isRight, hush)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Data.Array as Array
import Data.Array ((!!), unsafeIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Effect.Class (class MonadEffect)
import Effect.Console (logShow)
import Data.Traversable (sequence)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(Nil), (:))
import Data.NonEmpty ((:|))
import Data.MediaType (MediaType(MediaType))
import Data.Int as Int
import Data.Tuple (Tuple(..), fst)
import Data.String.Common (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Web.Event.Event as Event
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.HTML.Event.DragEvent as DragEvent
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DataTransfer as DataTransfer
import Util (moveWithin)
import Parser (parseFormula)
import Proof as P
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

data RuleType
  = RtPremise
  | RtAssumption
  | AndElim1
  | AndElim2
  | AndIntro
  | ImplElim
  | ImplIntro
  | BottomElim
  | DoubleNegElim
  | NegElim
  | ModusTollens
  | DoubleNegIntro

parseRuleText :: String -> Maybe RuleType
parseRuleText = case _ of
  "Premise" -> Just RtPremise
  "Ass." -> Just RtAssumption
  "∧e1" -> Just AndElim1
  "∧e2" -> Just AndElim2
  _ -> Nothing

parseRule :: ProofRow -> Maybe P.Rule
parseRule { rule, ruleArgs } = case rule, ruleArgs of
  Assumption _, [] -> Just P.Assumption
  _, _ -> Nothing

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
  | RowKeyEvent Int KeyboardEvent
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

-- | Tree representation of a ND proof,
-- |
-- | where the internal nodes are boxes and the leafs are rows.
data ProofTree
  = Subproof (Array ProofTree)
  | RowNode Int ProofRow

-- | Converts the GUI proof representation into an explicit tree structure.
proofTree :: State -> Array ProofTree
proofTree { rows } = case result of
  root :| Nil -> root.elems
  _ -> unsafeCrashWith "Unclosed boxes"
  where
  result =
    foldlWithIndex
      ( \i (currentBox@{ elems } :| parentBoxes) proofRow ->
          closeBoxesIfPossible i case proofRow.rule of
            Assumption { boxEndIdx } ->
              { elems: [ RowNode i proofRow ], endIdx: boxEndIdx }
                :| currentBox
                : parentBoxes
            _ -> (currentBox { elems = Array.snoc elems $ RowNode i proofRow }) :| parentBoxes
      )
      ({ elems: [], endIdx: Array.length rows } :| Nil)
      rows

  closeBoxesIfPossible i = case _ of
    { endIdx } :| _
      | endIdx < i -> unsafeCrashWith "Unreachable (box ends outside of parent)"
    { elems: currentElems, endIdx } :| parent : rest
      | endIdx == i ->
        closeBoxesIfPossible i
          $ parent
              { elems = Array.snoc parent.elems $ Subproof currentElems }
          :| rest
    x -> x

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render st =
  HH.div [ HP.classes [ HH.ClassName "proof" ] ]
    [ proofHeader, proofRows ]
  where
  proofHeader =
    HH.div
      [ HP.classes [ HH.ClassName "proof-header" ] ]
      [ HH.span [] [ HH.text premises, HH.text " ⊢ " ]
      , HH.slot _symbolInput (-1) (symbolInput "Conclusion") st.conclusion UpdateConclusion
      ]

  premises =
    joinWith ", "
      $ _.formulaText
      <$> Array.takeWhile ((_ == Premise) <<< _.rule) st.rows

  proofRows :: HH.HTML _ _
  proofRows =
    let
      renderProofTree = case _ of
        Subproof xs -> HH.div [ HP.classes [ HH.ClassName "proof-box" ] ] (renderProofTree <$> xs)
        RowNode i r -> row i r
    in
      HH.div
        [ HP.classes [ HH.ClassName "proof-rows" ] ]
        (renderProofTree <$> proofTree st)

  parsedRows = parseFormula <<< _.formulaText <$> st.rows

  verification =
    let
      proofTreeAction :: ProofTree -> Array (P.ND Unit)
      proofTreeAction = case _ of
        Subproof xs -> [ P.openBox ] <> (xs >>= proofTreeAction) <> [ P.closeBox ]
        RowNode i r ->
          pure
            $ P.addProof
                { formula: hush $ unsafePartial $ parsedRows `unsafeIndex` i
                , rule: parseRule r
                }

      conclusion = hush $ parseFormula st.conclusion
    in
      P.runND conclusion (sequence $ proofTree st >>= proofTreeAction)

  row :: Int -> ProofRow -> HH.HTML _ _
  row i { formulaText, rule } =
    let
      isFormulaOk = isRight $ unsafePartial $ parsedRows `unsafeIndex` i
    in
      HH.div
        [ HP.classes
            ( [ HH.ClassName "columns", HH.ClassName "is-mobile", HH.ClassName "proof-row" ]
                <> maybe []
                    (\j -> if i == j then [ HH.ClassName "dragged-over" ] else [])
                    st.draggingOver
                <> if isFormulaOk then [] else [ HH.ClassName "incorrect-formula" ]
            )
        , HP.draggable true
        , HE.onKeyDown $ RowKeyEvent i
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
              [ HH.slot _symbolInput (2 * i) (symbolInput "Enter formula") formulaText (UpdateFormula i) ]
          , HH.div
              [ HP.classes [ HH.ClassName "column", HH.ClassName "is-narrow" ] ]
              [ HH.span
                  [ HP.classes [ HH.ClassName "rule-field" ] ]
                  [ HH.slot _symbolInput (2 * i + 1) (symbolInput "Rule") (ruleText rule) (UpdateRule i) ]
              ]
          ]
        )

-- | The media type for the index of a proof row as a string.
rowMediaType :: MediaType
rowMediaType = MediaType "application/x.row"

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  UpdateFormula i s -> H.modify_ \st -> st { rows = unsafePartial $ fromJust $ Array.modifyAt i _ { formulaText = s } st.rows }
  UpdateRule i s -> do
    H.modify_ \st ->
      st
        { rows =
          unsafePartial $ fromJust
            $ Array.modifyAt i _ { rule = ruleFromString s i }
                st.rows
        }
    H.tell _symbolInput (2 * i + 1) SI.Focus
  RowKeyEvent i ev -> case KeyboardEvent.key ev of
    "Enter" -> if KeyboardEvent.shiftKey ev then exitBox i else addRowBelow i
    _ -> pure unit
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
  addRowBelow i = do
    H.modify_ \st ->
      st
        { rows =
          unsafePartial $ fromJust $ Array.insertAt (i + 1) emptyRow
            $ incrBoxEnds i st.rows
        }
    H.tell _symbolInput (2 * (i + 1)) SI.Focus -- Focus the newly added row

  -- | Creates a new row directly below the current index. If the current
  --   index is at the end of a box, the new row is created outside the box.
  exitBox i = do
    H.modify_ \st ->
      st
        { rows =
          unsafePartial $ fromJust $ Array.insertAt (i + 1) emptyRow
            $ ( if i `Array.elem` (getAllEndings st) then
                  incrBoxEndsWithEx i (scopeStart i st)
                else
                  incrBoxEnds i
              )
                st.rows
        }
    H.tell _symbolInput (2 * (i + 1)) SI.Focus -- Focus the newly added row

  -- | Takes an index and an array of rows. Increases the ending position of
  --   all boxes which end after the given index by one.
  incrBoxEnds :: Int -> Array ProofRow -> Array ProofRow
  incrBoxEnds i =
    map \r -> case r of
      row@{ rule: Assumption { boxEndIdx } }
        | i <= boxEndIdx -> row { rule = Assumption { boxEndIdx: boxEndIdx + 1 } }
      x -> x

  -- | Takes an index and an array of rows. Increases the ending position of
  --   all boxes which end after the given index by one, except any box which
  --   starts at the exception position.
  incrBoxEndsWithEx :: Int -> Int -> Array ProofRow -> Array ProofRow
  incrBoxEndsWithEx i e rs =
    map
      ( \(Tuple idx r) ->
          if idx == e then
            r
          else case r of
            row@{ rule: Assumption { boxEndIdx } }
              | (i <= boxEndIdx) -> row { rule = Assumption { boxEndIdx: boxEndIdx + 1 } }
            x -> x
      )
      (indexRows rs)

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

-- | Takes an index and a state and returns the starting position of the
--   innermost box which contains the index.
scopeStart :: Int -> State -> Int
scopeStart i st = fst $ innermostScope i st
  where
  -- | Gets the innermost box in a state which contains the given index.
  innermostScope :: Int -> State -> Tuple Int Int
  innermostScope i st = unsafePartial $ fromJust $ Array.last $ inBoxes i (allBoxLimits st)

  -- | Gets a list of all the boxes which a given position is in.
  inBoxes :: Int -> Array (Tuple Int Int) -> Array (Tuple Int Int)
  inBoxes i bs = Array.filter (inBox i) bs

  -- | Checks if a position is inside a box.
  inBox :: Int -> Tuple Int Int -> Boolean
  inBox i (Tuple lo hi) = i >= lo && i <= hi

  -- | Extracts all boxes along with their index from a list of indexed rows.
  indexedBoxes :: State -> Array (Tuple Int ProofRow)
  indexedBoxes st = Array.filter (\(Tuple i r) -> isBox r) (indexRows st.rows)

  -- | Takes a row, assumed to be the start of a box, along with it's
  --   position in the proof. Returns a tuple with the limits of the box.
  boxLimits :: Tuple Int ProofRow -> Tuple Int Int
  boxLimits (Tuple i r) = case r.rule of
    (Assumption { boxEndIdx }) -> Tuple i boxEndIdx
    _ -> unsafeCrashWith "Not a box."

  -- | Get a list of all boxes in a state as tuples.
  allBoxLimits :: State -> Array (Tuple Int Int)
  allBoxLimits st = map boxLimits $ indexedBoxes st

-- | Tags each line in an array with it's index in the array.
indexRows :: forall t. Array t -> Array (Tuple Int t)
indexRows arr = Array.zip (0 Array... (Array.length arr)) arr

getAllEndings :: State -> Array Int
getAllEndings st = map rToE $ boxes st
  where
  rToE :: ProofRow -> Int
  rToE r = case r.rule of
    Assumption { boxEndIdx } -> boxEndIdx
    _ -> unsafeCrashWith "Row is not a box."

boxes :: State -> Array ProofRow
boxes st = Array.filter isBox st.rows

-- | Check if a row is the start of a box.
isBox :: ProofRow -> Boolean
isBox r = case r.rule of
  Assumption _ -> true
  _ -> false

ruleFromString :: String -> Int -> Rule
ruleFromString s rowIdx
  | s == "Ass." || s == "as" = Assumption { boxEndIdx: rowIdx }
  | s == "pr" || s == "Premise" = Premise
  | otherwise = Rule s
