module GUI.Proof (Query(..), proof) where

import Prelude
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Either (isRight, Either(..))
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Data.Array as Array
import Data.Array ((!!), unsafeIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Effect.Class (class MonadEffect)
import Effect.Console (logShow)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(Nil), (:))
import Data.NonEmpty ((:|))
import Data.MediaType (MediaType(MediaType))
import Data.Int as Int
import Data.Set (Set)
import Data.Set as Set
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
import GUI.SymbolInput as SI
import GUI.SymbolInput (symbolInput)
import GUI.Rules as R
import Formula (Formula(..))

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
    {- For elimination rules we need to be able to inspect the formula and extract the
       components that we need from it. This will be nothing if the formula in the field
       is either empty or ill-formed.
       The idea is that if this field is Nothing and the formulaText is not empty, we can
       do something to signal to the user that the formula is ill-formed. Red text is not
       visible for everyone, so perhaps something else. -}
    , formulaIR   :: Maybe Formula
    , rule        :: Rule
    , ruleArgs    :: Array String
    }

-- | A newly added row.
emptyRow :: ProofRow
emptyRow = { formulaText: "", formulaIR: Nothing, rule: Rule "", ruleArgs: [] }

-- | Only stores endpoints of boxes since assumptions naturally define start points.
type State
  = { premises     :: String
    , conclusion   :: String
    , rows         :: Array ProofRow
    , draggingOver :: Maybe Int
    , boxEnds      :: Set Int
    , expectsArgs  :: Int            -- How many more operands needs to be selected
    , clicked      :: Array Int      -- Which operands have already been selected
    , clickedRule  :: Maybe R.Rules  -- The rule that we want to apply the operands to
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
  {- Just to have something that works for now I have made it so that a user selects
     the operands to apply to a rule by clicking the row number. This action will notify
     the handleAction function which row was clicked. Perhaps this is not optimal, but
     this is a prototype so it can always be changed for something more suitable later.
     
     Ideally the number would be highlighted or something if you hover over it, to signal
     to the user that it can be clicked, but right now nothing happens. Those who know, knows! -}
  | ClickedRow Int

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

initialState _ =
  { premises: ""
  , conclusion: ""
  , rows: [ emptyRow ]
  , draggingOver: Nothing
  , boxEnds: Set.empty
  , expectsArgs: 0
  , clicked: []
  , clickedRule: Nothing
  }

-- When we end up here a button has been clicked in the rules panel
handleQuery :: forall a action output m. MonadEffect m => Query a -> H.HalogenM State action Slots output m (Maybe a)
handleQuery (Tell command a) = case command of
  R.AndElim1 -> do
    H.liftEffect $ logShow "and elim 1"
    {- When we enter one of these cases the user has chosen to apply a rule. However,
       we don't yet know which operands to apply the rule to. We modify the state to
       remember which rule was clicked and how many operands is required.
       
       Perhaps when we enter a state like this, all the other functionality in the
       editor could be disabled until the right amount of operands have been selected.
       Nothing stops a user from initiating an and introduction and then initiating an
       and elimination before the and introduction is complete. -}
    H.modify_ \st ->
      st
        { expectsArgs = 1
        , clicked = []
        , clickedRule = Just R.AndElim1
        }
    pure Nothing
  R.AndElim2 -> do
    H.liftEffect $ logShow "and elim 2"
    H.modify_ \st ->
      st
        { expectsArgs = 1
        , clicked = []
        , clickedRule = Just R.AndElim2
        }
    pure Nothing
  R.AndIntro -> do
    H.liftEffect $ logShow "and introduction"
    H.modify_ \st ->
      st
        { expectsArgs = 2
        , clicked = []
        , clickedRule = Just R.AndIntro
        }
    pure Nothing
  R.OrIntro -> do
    H.liftEffect $ logShow "or intro"
    H.modify_ \st ->
      st
        { expectsArgs = 2
        , clicked = []
        , clickedRule = Just R.OrIntro
        }
    pure Nothing
  R.NotIntro -> do
    H.liftEffect $ logShow "not intro"
    H.modify_ \st ->
      st
        { expectsArgs = 1 -- Actually this should be applied to a box and not just an operand, but I don't know how to do this now
        , clicked = []
        , clickedRule = Just R.NotIntro
        }
    pure Nothing
  R.NotElim -> do
    H.liftEffect $ logShow "not elim"
    H.modify_ \st ->
      st
        { expectsArgs = 1 -- Same as for not intro, should be a box
        , clicked = []
        , clickedRule = Just R.NotElim
        }
    pure Nothing

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
                  [ HP.classes [ HH.ClassName "title", HH.ClassName "row-index" ]
                  , HE.onClick \_ -> ClickedRow i
                  ]
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
  -- This gigantic case is run when a user has selected an operand
  ClickedRow i -> do
    H.liftEffect $ logShow $ "clicked row: " <> show i
    st <- H.get
    -- If this is _not_ the final operand, we just update the state accordingly (look below, in the then)
    if st.expectsArgs > 0 then
      -- If this is the last operand, however, we are ready to apply a rule and create a new row
      if st.expectsArgs == 1 then case st.clickedRule of
        Just R.AndIntro ->
          let
            h1 = unsafePartial $ fromJust $ Array.head $ st.clicked

            row1 = fetchRow st h1

            row2 = fetchRow st i

            {- Right now it is assumed that the selected operands are well-formed (and thus
               also parsed correctly), but we should actually have a check here to make sure
               that the operands are OK. We can and-intro any two formulas so we don't need to
               inspect what the formulas actually are. -}
            ir1 = unsafePartial $ fromJust $ row1.formulaIR

            ir2 = unsafePartial $ fromJust $ row2.formulaIR

            formula = And ir1 ir2

            newrow =
              { formulaText: show formula
              , formulaIR: Just formula
              , rule: Rule "∧i"
              , ruleArgs: [ show (h1 + 1), show (i + 1) ]
              }
            
            newrows1 = if (unsafePartial (fromJust (Array.last st.rows))) == emptyRow
                       then Array.snoc (unsafePartial $ fromJust (Array.init st.rows)) newrow
                       else Array.snoc st.rows newrow
            newrows2 = Array.snoc newrows1 emptyRow
          in
            do
              {- When we add the new row we have finished applying a rule, so we need to
                 also modify the state to not expect more operands etc. -}
              H.modify_ \st ->
                st
                  { expectsArgs = 0
                  , clicked = []
                  , clickedRule = Nothing
                  -- This adds the new row below the current rows, but if the last row is an empty row it looks weird to add this row underneath an empty row. Maybe we need to have a check to see if the last row is empty (maybe because the user had thought to write it in themselves but then decided to use a button instead?) and in that case replace the empty row with this new row?
                  , rows = newrows2
                  }
        Just R.OrIntro ->
          let
            h1 = unsafePartial $ fromJust $ Array.head $ st.clicked

            row1 = fetchRow st h1

            row2 = fetchRow st i

            ir1 = unsafePartial $ fromJust $ row1.formulaIR

            ir2 = unsafePartial $ fromJust $ row2.formulaIR

            formula = Or ir1 ir2

            {- This might be a little weird, will user actually apply OrIntro to a couple of rows? Isn't it usually so that you can pick anything to be introduced on the one of the sides of an OrIntro -
            i.e. 
             row 1   A                Premise
             row 2   A ∨ "anything"   ∨i, 1 -}
            newrow =
              { formulaText: show formula -- not sure if this renders properly, lack of parentheses
              , formulaIR: Just formula
              , rule: Rule "∨i"
              , ruleArgs: [ show (h1 + 1), show (i + 1) ]
              }
          in
            do
              H.modify_ \st ->
                st
                  { expectsArgs = 0
                  , clicked = []
                  , clickedRule = Nothing
                  , rows = Array.snoc st.rows newrow
                  }
        Just R.NotIntro ->
          let
            row = fetchRow st i

            f = row.formulaText

            ir = unsafePartial $ fromJust $ row.formulaIR

            formula = Not ir

            newrow =
              { formulaText: show formula
              , formulaIR: Just formula
              , rule: Rule "¬i"
              , ruleArgs: [ show (i + 1) ]
              }
          in
            do
              H.modify_ \st ->
                st
                  { expectsArgs = 0
                  , clicked = []
                  , clickedRule = Nothing
                  , rows = Array.snoc st.rows newrow
                  }
        Just R.AndElim1 -> do
          st <- H.get
          let
            row = fetchRow st i
          {- When we apply the elimination rules we need to check that we are performing
             valid eliminations. In this example, to perform an and elimination the operand has
             to be a conjunction. -}
          case row.formulaIR of
            Just (And e1 _) ->
              let
                newrow =
                  { formulaText: show e1
                  , formulaIR: Just e1
                  , rule: Rule "∧e1"
                  , ruleArgs: [ show (i + 1) ]
                  }
              in
                H.modify_ \st ->
                  st
                    { expectsArgs = 0
                    , clicked = []
                    , clickedRule = Nothing
                    , rows = Array.snoc st.rows newrow
                    }
            -- This is when someone tries to and-elim something that is not a conjunction
            Just _ -> pure unit
            -- This is when someone tries to and-elim an illformed formula. We should report errors here.
            Nothing -> pure unit
        Just R.AndElim2 -> do
          st <- H.get
          let
            row = fetchRow st i
          case row.formulaIR of
            Just (And _ e2) ->
              let
                newrow =
                  { formulaText: show e2
                  , formulaIR: Just e2
                  , rule: Rule "∧e2"
                  , ruleArgs: [ show (i + 1) ]
                  }
              in
                H.modify_ \st ->
                  st
                    { expectsArgs = 0
                    , clicked = []
                    , clickedRule = Nothing
                    , rows = Array.snoc st.rows newrow
                    }
            Just _ -> pure unit
            Nothing -> pure unit
        _ -> pure unit
      else
        H.modify_ \st ->
          st
            { expectsArgs = st.expectsArgs - 1
            , clicked = Array.snoc st.clicked i
            }
    else
      -- If the user clicked an operand when we are not expecting any we just don't do anything
      pure unit
  UpdateFormula i s -> do
    st <- H.get
    let
      row = unsafePartial $ fromJust $ Array.index st.rows i
    case parseFormula s of
      Left err ->
        let
          newrows = myupdateAt i st.rows (row { formulaText = s, formulaIR = Nothing })
        in
          H.modify_ \st -> st { rows = newrows }
      Right formula ->
        let
          newrows = myupdateAt i st.rows (row { formulaText = s, formulaIR = Just formula })
        in
          H.modify_ \st -> st { rows = newrows }
  UpdateRule i s ->
    H.modify_ \st ->
      st
        { rows =
          unsafePartial $ fromJust
            $ Array.modifyAt i _ { rule = ruleFromString s i }
                st.rows
        }
  RowKeyEvent i ev -> case KeyboardEvent.key ev of
    "Enter" -> addRowBelow i
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
  _ -> pure unit
  where
  addRowBelow i = do
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
    H.tell _symbolInput (2 * (i + 1)) SI.Focus -- Focus the newly added row

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

  -- I hate writing so many unsafePartial $ fromJust's
  fetchRow st row = unsafePartial $ fromJust $ Array.index st.rows row

  myupdateAt :: forall a. Int -> Array a -> a -> Array a
  myupdateAt i arr a = unsafePartial $ fromJust $ Array.updateAt i a arr

ruleFromString :: String -> Int -> Rule
ruleFromString s rowIdx
  | s == "Ass." || s == "as" = Assumption { boxEndIdx: rowIdx }
  | s == "pr" || s == "Premise" = Premise
  | otherwise = Rule s
