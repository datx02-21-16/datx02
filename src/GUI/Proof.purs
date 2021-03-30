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
import Data.Tuple (Tuple(..), fst)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
  show (Assumption { boxEndIdx }) = "Assumption (box ends at " <> show boxEndIdx <> ")"

ruleText :: Rule -> String
ruleText (Rule s) = s

ruleText Premise = "Premise"

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
                                      $ HH.div
                                          [ HP.classes [ HH.ClassName "proof-box" ] ]
                                          currentElems
                                  }
                              :| rest
                        x -> x
                    in
                      closeBoxesIfPossible case proofRow.rule of
                        Rule s ->
                          ( currentBox
                              { elems =
                                Array.snoc elems
                                  $ row i proofRow
                              }
                          )
                            :| parentBoxes
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
    -- | Creates a new row directly below the given index.
    createBelow :: Int -> State -> State
    createBelow i st =
      st
        { rows =
          unsafePartial
            $ fromJust
            $ Array.insertAt (i + 1) emptyRow
            $ (incrBoxEnds i) st.rows
        }

    -- | Creates a new row directly below the current index. If the current
    --   index is at the end of a box, the new row is created outside the box.
    exitBox :: Int -> State -> State
    exitBox i st =
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

    -- | Takes an index and an array of rows. Increases the ending position of
    --   all boxes which end after the given index by one.
    incrBoxEnds :: Int -> Array Row -> Array Row
    incrBoxEnds i =
      map \r -> case r of
        row@{ rule: Assumption { boxEndIdx } }
          | i <= boxEndIdx -> row { rule = Assumption { boxEndIdx: boxEndIdx + 1 } }
        x -> x

    -- | Takes an index and an array of rows. Increases the ending position of
    --   all boxes which end after the given index by one, except any box which
    --   starts at the exception position.
    incrBoxEndsWithEx :: Int -> Int -> Array Row -> Array Row
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
    indexedBoxes :: State -> Array (Tuple Int Row)
    indexedBoxes st = Array.filter (\(Tuple i r) -> isBox r) (indexRows st.rows)

    -- | Takes a row, assumed to be the start of a box, along with it's
    --   position in the proof. Returns a tuple with the limits of the box.
    boxLimits :: Tuple Int Row -> Tuple Int Int
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
    rToE :: Row -> Int
    rToE r = case r.rule of
      Assumption { boxEndIdx } -> boxEndIdx
      _ -> unsafeCrashWith "Row is not a box."

  boxes :: State -> Array Row
  boxes st = Array.filter isBox st.rows

  -- | Check if a row is the start of a box.
  isBox :: Row -> Boolean
  isBox r = case r.rule of
    Assumption _ -> true
    _ -> false

  ruleFromString :: String -> Int -> Rule
  ruleFromString s rowIdx
    | s == "Ass." || s == "as" = Assumption { boxEndIdx: rowIdx }
    | otherwise = Rule s
