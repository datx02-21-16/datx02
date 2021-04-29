module GUI.Proof (Slot, proof) where

import Prelude
import Data.Array ((!!), unsafeIndex)
import Data.Array as Array
import Data.Either (isRight, either, hush)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust, maybe)
import Data.NonEmpty ((:|))
import Data.String (Pattern(Pattern), split, trim, joinWith)
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple), fst)
import Effect.Class (class MonadEffect)
import GUI.Rules (RuleType(..))
import GUI.Rules as R
import GUI.SymbolInput (symbolInput)
import GUI.SymbolInput as SI
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPARIA
import Parser (parseFormula, parsePremises)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Proof (NdError(..))
import Proof as P
import Type.Proxy (Proxy(..))
import GUI.Hint as Hint
import Util (enumerate, moveWithin)
import Web.Event.Event as Event
import Web.HTML.Event.DataTransfer as DataTransfer
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent as DragEvent
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Text.Parsing.Parser (parseErrorMessage)

-- For GUI proof state we use a representation that is easy to modify,
-- i.e. has a single contiguous array of all rows. When rendering or
-- validating we map this to a tree with subproof nodes.
data Rule
  = Rule String
  | Premise
  | Assumption
    { boxEndIdx :: Int -- Inclusive end of box
    }
  | Fresh { boxEndIdx :: Int }

derive instance eqRule :: Eq Rule

instance showRule :: Show Rule where
  show (Rule s) = s
  show Premise = "Premise"
  show (Assumption { boxEndIdx }) = "Assumption (box ends at " <> show boxEndIdx <> ")"

data RuleArgType
  = RowIdx
  | BoxRange

-- | Given a rule, returns specification of the number and types of arguments it expects.
ruleArgTypes :: R.RuleType -> Array RuleArgType
ruleArgTypes = case _ of
  RtPremise -> []
  RtAssumption -> []
  AndElim1 -> [ RowIdx ]
  AndElim2 -> [ RowIdx ]
  AndIntro -> [ RowIdx, RowIdx ]
  OrElim -> [ RowIdx, BoxRange, BoxRange ]
  OrIntro1 -> [ RowIdx ]
  OrIntro2 -> [ RowIdx ]
  ImplElim -> [ RowIdx, RowIdx ]
  ImplIntro -> [ BoxRange ]
  NegElim -> [ RowIdx, RowIdx ]
  NegIntro -> [ BoxRange ]
  BottomElim -> [ RowIdx ]
  DoubleNegElim -> [ RowIdx ]
  ModusTollens -> [ RowIdx, RowIdx ]
  DoubleNegIntro -> [ RowIdx ]
  PBC -> [ BoxRange ]
  LEM -> []
  RtCopy -> [ RowIdx ]
  RtFresh -> []
  ForallElim -> [ RowIdx ]
  ForallIntro -> [ BoxRange ]
  ExistsElim -> [ RowIdx, BoxRange ]
  ExistsIntro -> [ RowIdx ]
  EqElim -> [ RowIdx, RowIdx ]
  EqIntro -> []

parseRowIdx :: String -> Maybe Int
parseRowIdx = Int.fromString

parseBoxRange :: String -> Maybe P.Box
parseBoxRange s = case Int.fromString <$> split (Pattern "-") s of
  [ Just i, Just j ] -> Just $ Tuple i j
  _ -> Nothing

parseRuleText :: String -> Maybe RuleType
parseRuleText = case _ of
  "Premise" -> Just RtPremise
  "Ass." -> Just RtAssumption
  "∧e1" -> Just AndElim1
  "∧e2" -> Just AndElim2
  "∧i" -> Just AndIntro
  "∨e" -> Just OrElim
  "∨i1" -> Just OrIntro1
  "∨i2" -> Just OrIntro2
  "→e" -> Just ImplElim
  "→i" -> Just ImplIntro
  "¬e" -> Just NegElim
  "¬i" -> Just NegIntro
  "⊥e" -> Just BottomElim
  "¬¬e" -> Just DoubleNegElim
  "MT" -> Just ModusTollens
  "¬¬i" -> Just DoubleNegIntro
  "PBC" -> Just PBC
  "LEM" -> Just LEM
  "Copy" -> Just RtCopy
  "Fresh" -> Just RtFresh
  "∀i" -> Just ForallIntro
  "∃e" -> Just ExistsElim
  "∃i" -> Just ExistsIntro
  "=e" -> Just EqElim
  "=i" -> Just EqIntro
  _ -> Nothing

parseRule :: ProofRow -> Maybe P.Rule
parseRule { rule, ruleArgs } =
  parseRuleText (ruleText rule)
    >>= \ruleType ->
        let
          argCount = Array.length (ruleArgTypes ruleType)

          args = Array.take argCount $ ruleArgs <> Array.replicate argCount ""
        in
          case ruleType, args of
            RtAssumption, [] -> Just P.Assumption
            RtPremise, [] -> Just P.Premise
            AndElim1, [ a ] -> Just $ P.AndElim1 (parseRowIdx a)
            AndElim2, [ a ] -> Just $ P.AndElim2 (parseRowIdx a)
            AndIntro, [ a, b ] -> Just $ P.AndIntro (parseRowIdx a) (parseRowIdx b)
            OrElim, [ a, b, c ] -> Just $ P.OrElim (parseRowIdx a) (parseBoxRange b) (parseBoxRange c)
            OrIntro1, [ a ] -> Just $ P.OrIntro1 (parseRowIdx a)
            OrIntro2, [ a ] -> Just $ P.OrIntro2 (parseRowIdx a)
            ImplElim, [ a, b ] -> Just $ P.ImplElim (parseRowIdx a) (parseRowIdx b)
            ImplIntro, [ a ] -> Just $ P.ImplIntro (parseBoxRange a)
            NegElim, [ a, b ] -> Just $ P.NegElim (parseRowIdx a) (parseRowIdx b)
            NegIntro, [ a ] -> Just $ P.NegIntro (parseBoxRange a)
            BottomElim, [ a ] -> Just $ P.BottomElim (parseRowIdx a)
            DoubleNegElim, [ a ] -> Just $ P.DoubleNegElim (parseRowIdx a)
            ModusTollens, [ a, b ] -> Just $ P.ModusTollens (parseRowIdx a) (parseRowIdx b)
            DoubleNegIntro, [ a ] -> Just $ P.DoubleNegIntro (parseRowIdx a)
            PBC, [ a ] -> Just $ P.PBC (parseBoxRange a)
            LEM, [] -> Just P.LEM
            RtCopy, [ a ] -> Just $ P.Copy (parseRowIdx a)
            RtFresh, [] -> Just P.Fresh
            _, _ -> Nothing

ruleText :: Rule -> String
ruleText (Rule s) = s

ruleText Premise = "Premise"

ruleText (Assumption _) = "Ass."

ruleText (Fresh _) = "Fresh"
errorText :: P.NdError -> String
errorText = case _ of
  P.BadRef -> "Reference to invalid row"
  P.RefDiscarded -> "Reference to row in discarded box"
  P.RefOutOfBounds -> "Reference to non-existent row"
  P.BadRule -> "Bad rule application"
  P.BadFormula -> "No formula can be parsed"
  P.FormulaMismatch -> "Formula does not match output from rule"
  P.InvalidRule -> "Non-existent rule"
  P.NotABox -> "Not a valid box"

type ProofRow
  = { formulaText :: String
    , rule :: Rule
    , ruleArgs :: Array String
    }

-- | A newly added row.
emptyRow :: ProofRow
emptyRow = { formulaText: "", rule: Rule "", ruleArgs: [] }

type Slot id
  = forall query output. H.Slot query output id

-- | Only stores endpoints of boxes since assumptions naturally define start points.
type State
  = { conclusion :: String
    , rows :: Array ProofRow
    , draggingOver :: Maybe Int
    , dragged :: Maybe Int
    , inFocus :: Int
    -- Store user premises field input in order to not overwrite it
    -- after updating "rows", unless it is invalidated (if user edits
    -- a premise proof row instead).
    , premisesInput :: String
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
  | UpdateRuleArg Int Int String
  | FormulaKeyDown Int KeyboardEvent
  | ClearProof
  | ShowHint
  | UpdatePremises String
  | AddBelow
  | AddOutsideBox
  | SetFocus Int

_symbolInput = Proxy :: Proxy "symbolInput"

type Slots
  = ( symbolInput :: SI.Slot Int )

proof :: forall query input output m. MonadEffect m => H.Component query input output m
proof =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            }
    }

initialState :: forall input. input -> State
initialState _ =
  { conclusion: ""
  , rows: [ emptyRow ]
  , draggingOver: Nothing
  , dragged: Nothing
  , inFocus: 0
  , premisesInput: ""
  }

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
            Fresh { boxEndIdx } ->
              { elems: [ RowNode i proofRow ], endIdx: boxEndIdx }
                :| currentBox
                : parentBoxes
            _ -> (currentBox { elems = Array.snoc elems $ RowNode i proofRow }) :| parentBoxes
      )
      ({ elems: [], endIdx: Array.length rows } :| Nil)
      rows

  closeBoxesIfPossible i = case _ of
    { endIdx } :| _
      | endIdx < i -> unsafeCrashWith "Unreachable (nested box ended outside of parent)"
    { elems: currentElems, endIdx } :| parent : rest
      | endIdx == i ->
        closeBoxesIfPossible i
          $ parent
              { elems = Array.snoc parent.elems $ Subproof currentElems }
          :| rest
    x -> x

premises :: Array ProofRow -> Array String
premises = map _.formulaText <<< Array.takeWhile ((_ == Premise) <<< _.rule)

strToPremiseRows :: String -> Array ProofRow
strToPremiseRows =
  map { formulaText: _, rule: Premise, ruleArgs: [] }
    <<< removeEmptyString
    <<< Array.fromFoldable
    <<< parsePremises
  where
  removeEmptyString [ "" ] = []

  removeEmptyString x = x

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action Slots m
render st =
  HH.div
    [ HP.classes [ H.ClassName "panel", H.ClassName "is-primary" ] ]
    [ HH.p
        [ HP.classes [ H.ClassName "panel-heading" ] ]
        [ HH.text "Proof" ]
    , toolbar
    , HH.div
        [ HP.classes $ [ H.ClassName "panel-block", H.ClassName "proof" ]
            <> if complete then [ H.ClassName "complete" ] else []
        ]
        [ proofHeader
        , proofRows
        ]
    ]
  where
  proofHeader :: HH.HTML _ _
  proofHeader =
    HH.div
      [ HP.classes
          [ H.ClassName "columns"
          , H.ClassName "is-mobile"
          , H.ClassName "proof-header"
          , H.ClassName "is-gapless"
          ]
      , HE.onFocusOut $ const $ SetFocus (Array.length st.rows - 1)
      ]
      [ premiseDisplay
      , turnstile
      , formulaField
          { i: (-1)
          , placeholder: "Conclusion"
          , text: st.conclusion
          , outputMap: UpdateConclusion
          , classes:
              [ H.ClassName "column"
              , H.ClassName "is-half"
              , H.ClassName "conclusion-field"
              ]
          }
      ]

  toolbar :: HH.HTML _ _
  toolbar =
    HH.nav [ HPARIA.role "toolbar", HP.classes [ H.ClassName "level", H.ClassName "is-mobile" ] ]
      [ HH.div [ HP.classes [ H.ClassName "level-left" ] ] [ addRowButton, addRowOutsideButton ]
      , HH.div [ HP.classes [ H.ClassName "level-right" ] ]
          [ HH.div [ HP.classes [ H.ClassName "level-item" ] ] [ clearButton, hintButton ] ]
      ]

  addRowButton :: HH.HTML _ _
  addRowButton = toolbarButton "Add row" "Add a new row below the row currently in focus, stay in the current box." AddBelow

  addRowOutsideButton :: HH.HTML _ _
  addRowOutsideButton = toolbarButton "Add row (with box exit)" "Add a new row below the row currently in focus, exit the current box if the current line is at the end of the box." AddOutsideBox

  clearButton :: HH.HTML _ _
  clearButton = toolbarButton "Clear" "Erase all rows from the current proof." ClearProof

  hintButton :: HH.HTML _ _
  hintButton = toolbarButton "Hint" "Get a hint on how to get started." ShowHint

  toolbarButton :: String -> String -> Action -> HH.HTML _ _
  toolbarButton buttonText buttonTitle buttonAction =
    HH.button
      [ HP.classes [ H.ClassName "button", H.ClassName "is-white", H.ClassName "is-small" ]
      , HE.onClick $ const buttonAction
      , HP.title buttonTitle
      ]
      [ HH.text buttonText ]

  -- | Displays the premises in the header.
  premiseDisplay :: HH.HTML _ _
  premiseDisplay =
    let
      -- | All premises used in the proof as a string.
      premisesText
        | premises (strToPremiseRows st.premisesInput) == premises st.rows = st.premisesInput
        | otherwise = joinWith ", " $ premises st.rows
    in
      HH.span
        [ HP.classes
            [ H.ClassName "column"
            , H.ClassName "premises"
            ]
        ]
        [ HH.slot _symbolInput (-1) (symbolInput "Premises") premisesText UpdatePremises
        , HH.p [ HP.classes [ H.ClassName "help", H.ClassName "is-danger" ] ] []
        ]

  turnstile =
    HH.span [ HP.classes [ H.ClassName "column", H.ClassName "is-1" ] ]
      [ HH.input
          [ HP.classes
              [ H.ClassName "input"
              , H.ClassName "is-static"
              , H.ClassName "has-text-centered"
              ]
          , HP.readOnly true
          , HP.tabIndex (-1)
          , HP.value "⊢"
          ]
      , HH.p [ HP.classes [ H.ClassName "help", H.ClassName "is-danger" ] ] []
      ]

  -- | Renders an input field that verifies the parsability of the inputted formula.
  formulaField ::
    { i :: Int
    , placeholder :: String
    , text :: String
    , outputMap :: String -> Action
    , classes :: Array H.ClassName
    } ->
    HH.HTML _ Action
  formulaField { i, placeholder, text, outputMap, classes } =
    HH.span
      [ HP.classes $ [ H.ClassName "formula-field" ]
          <> classes
          <> if isOk then [] else [ H.ClassName "invalid" ]
      , HE.onKeyDown $ FormulaKeyDown i
      ]
      ( [ HH.slot _symbolInput (2 * i) (symbolInput placeholder) text outputMap ]
          <> [ HH.p [ HP.classes [ H.ClassName "help", H.ClassName "is-danger" ] ] (either (\err -> [ HH.text $ "Cannot parse formula: " <> parseErrorMessage err ]) (const []) parseResult) ]
      )
    where
    parseResult = parseFFC text

    isOk = isRight parseResult

  Tuple complete verification =
    let
      proofTreeAction :: ProofTree -> Array (P.ND Unit)
      proofTreeAction = case _ of
        Subproof xs -> [ P.openBox ] <> (xs >>= proofTreeAction) <> [ P.closeBox ]
        RowNode _ r ->
          pure
            $ P.addProof
                { formula: hush $ parseFFC r.formulaText
                , rule: parseRule r
                }

      conclusion = hush $ parseFFC st.conclusion
    in
      P.runND conclusion (sequence $ proofTree st >>= proofTreeAction)

  proofRows :: HH.HTML _ _
  proofRows =
    let
      renderProofTree = case _ of
        Subproof xs -> HH.div [ HP.classes [ H.ClassName "proof-box" ] ] (renderProofTree <$> xs)
        RowNode i r -> row i r
    in
      HH.div
        [ HP.classes [ H.ClassName "proof-rows" ] ]
        (renderProofTree <$> proofTree st)

  row :: Int -> ProofRow -> HH.HTML _ _
  row i { formulaText, rule, ruleArgs } =
    HH.div
      [ HP.classes
          ( [ H.ClassName "columns", H.ClassName "is-mobile", H.ClassName "proof-row" ]
              <> maybe []
                  (\j -> if i == j then [ H.ClassName "dragged-over" ] else [])
                  st.draggingOver
          )
      , HP.draggable true
      , HE.onKeyDown $ RowKeyEvent i
      , HE.onDragStart $ DragStart i
      , HE.onDragOver $ DragOver i
      , HE.onDragEnter $ DragEnter i
      , HE.onDragLeave $ DragLeave i
      , HE.onDrop $ Drop i
      , HE.onDragEnd $ DragEnd i
      , HE.onFocusIn $ const $ SetFocus i
      ]
      [ rowIndex
      , formulaField
          { i
          , placeholder: "Enter formula"
          , text: formulaText
          , outputMap: UpdateFormula i
          , classes: [ H.ClassName "column" ]
          }
      , ruleDisplay
      ]
    where
    error = (unsafePartial $ verification.rows `unsafeIndex` i).error

    -- | Displays the row index.
    rowIndex :: HH.HTML _ _
    rowIndex =
      HH.span
        [ HP.classes [ H.ClassName "column", H.ClassName "is-narrow" ] ]
        [ HH.h4
            [ HP.classes [ H.ClassName "title", H.ClassName "row-index" ] ]
            [ HH.text (show (1 + i)) ]
        ]

    ruleDisplay :: HH.HTML _ _
    ruleDisplay =
      HH.span [ HP.classes [ H.ClassName "column is-half" ] ]
        [ HH.div [ HP.classes [ H.ClassName "columns is-gapless" ] ] ([ ruleField ] <> argFields) ]

    ruleField :: HH.HTML _ _
    ruleField =
      HH.span
        [ HP.classes ([ H.ClassName "column rule-field" ] <> if isRuleError error then [ H.ClassName "invalid" ] else []) ]
        ( [ HH.slot _symbolInput (2 * i + 1) (symbolInput "Rule") (ruleText rule) (UpdateRule i) ]
            <> [ HH.p [ HP.classes [ H.ClassName "help", H.ClassName "is-danger" ] ]
                  (if isRuleError error then [ HH.text $ errorText (unsafePartial $ fromJust error) ] else [])
              ]
        )
      where
      isRuleError err = case err of
        Nothing -> false
        Just e -> case e of
          BadFormula -> false
          _ -> true

    argField :: Tuple Int (Tuple RuleArgType String) -> HH.HTML _ _
    argField (Tuple j (Tuple ruleArgType s)) =
      HH.span [ HP.classes [ H.ClassName "column", H.ClassName "is-narrow" ] ]
        [ HH.input
            [ HP.classes
                ( [ H.ClassName "input", H.ClassName "arg-field" ]
                    <> if isOk then [ H.ClassName "is-primary" ] else [ H.ClassName "is-danger" ]
                )
            , HP.value s
            , HP.placeholder placeholder
            , HE.onValueInput $ UpdateRuleArg i j
            ]
        ]
      where
      placeholder = case ruleArgType of
        RowIdx -> "Row"
        BoxRange -> "Box"

      isOk = case ruleArgType of
        RowIdx -> isJust $ parseRowIdx s
        BoxRange -> isJust $ parseBoxRange s

    argFields =
      fromMaybe [] do
        ruleType <- parseRuleText (ruleText rule)
        let
          argTypes = ruleArgTypes ruleType

          argStrings = ruleArgs <> Array.replicate (Array.length argTypes) ""
        pure $ argField <$> enumerate (Array.zip argTypes argStrings)

handleAction ::
  forall output m.
  MonadEffect m =>
  Action ->
  H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  UpdateFormula i s ->
    H.modify_ \st ->
      st
        { rows =
          unsafePartial
            $ fromJust
            $ Array.modifyAt i _ { formulaText = s } st.rows
        }
  UpdateRule i s -> do
    H.modify_ \st ->
      st
        { rows =
          unsafePartial $ fromJust
            $ Array.modifyAt i _ { rule = ruleFromString s i }
                st.rows
        }
    H.tell _symbolInput (2 * i + 1) SI.Focus
  UpdateRuleArg i j s ->
    H.modify_ \st ->
      st
        { rows =
          unsafePartial $ fromJust
            $ Array.modifyAt i
                ( \row ->
                    row
                      { ruleArgs =
                        unsafePartial $ fromJust
                          $ Array.modifyAt j (const s)
                              ( row.ruleArgs
                                  <> Array.replicate (max 0 (j + 1 - Array.length row.ruleArgs)) ""
                              )
                      }
                )
                st.rows
        }
  RowKeyEvent i ev -> case KeyboardEvent.key ev of
    "Enter"
      | KeyboardEvent.shiftKey ev -> exitBox i
    "Enter" -> addRowBelow i
    "Delete"
      | KeyboardEvent.shiftKey ev -> deleteRow i
    _ -> pure unit
  FormulaKeyDown i ev -> case KeyboardEvent.key ev of
    "Backspace"
      | i >= 0 -> do
        let
          target =
            unsafePartial $ fromJust
              $ Event.target (KeyboardEvent.toEvent ev)
              >>= HTMLInputElement.fromEventTarget
        value <- H.liftEffect $ HTMLInputElement.value target
        when (String.null value) do
          deleteRow i
          H.liftEffect $ Event.preventDefault (KeyboardEvent.toEvent ev)
    _ -> pure unit
  DragStart i _ -> do
    H.modify_ (\st -> st { dragged = Just i })
  DragOver i ev -> do
    validDropZone <- isValidDropZone i
    when validDropZone do
      H.modify_ \st -> st { draggingOver = Just i }
      H.liftEffect $ Event.preventDefault $ DragEvent.toEvent ev
  DragEnter i ev -> do
    validDropZone <- isValidDropZone i
    when validDropZone do
      H.liftEffect $ DataTransfer.setDropEffect DataTransfer.Move $ DragEvent.dataTransfer ev
      H.modify_ \st -> st { draggingOver = Just i }
  DragLeave i ev -> do
    draggingOver <- H.gets _.draggingOver
    when (draggingOver /= Just i) do
      H.liftEffect $ DataTransfer.setDropEffect DataTransfer.None $ DragEvent.dataTransfer ev
      H.modify_ \st -> st { draggingOver = Nothing }
  DragEnd _ _ -> H.modify_ \st -> st { draggingOver = Nothing, dragged = Nothing }
  Drop i ev -> do
    validDropZone <- isValidDropZone i
    when validDropZone do
      H.liftEffect $ Event.preventDefault $ DragEvent.toEvent ev
      { start, end } <- draggedRows
      H.modify_ \st ->
        let
          target = i + 1

          newStart = target - if start < target then end - start else 0

          updateBoxes =
            mapWithIndex \j -> case _ of
              row@{ rule: Assumption { boxEndIdx } }
                -- Moved box
                | start <= j, j < end ->
                  row
                    { rule = Assumption { boxEndIdx: boxEndIdx + (newStart - start) }
                    }
                -- Move before/inside box
                | i <= boxEndIdx, boxEndIdx < start ->
                  row
                    { rule = Assumption { boxEndIdx: boxEndIdx + (end - start) }
                    }
                -- Move after box
                | start <= boxEndIdx, boxEndIdx < i ->
                  row
                    { rule = Assumption { boxEndIdx: boxEndIdx - (end - start) }
                    }
              row@{ rule: Fresh { boxEndIdx } }
                -- Moved box
                | start <= j, j < end ->
                  row
                    { rule = Fresh { boxEndIdx: boxEndIdx + (newStart - start) }
                    }
                -- Move before/inside box
                | i <= boxEndIdx, boxEndIdx < start ->
                  row
                    { rule = Fresh { boxEndIdx: boxEndIdx + (end - start) }
                    }
                -- Move after box
                | start <= boxEndIdx, boxEndIdx < i ->
                  row
                    { rule = Fresh { boxEndIdx: boxEndIdx - (end - start) }
                    }
              x -> x

          rows' = moveWithin target start end $ updateBoxes st.rows
        in
          st { rows = rows' }
  UpdatePremises s ->
    H.modify_ \st ->
      let
        newRows = case strToPremiseRows s <> Array.dropWhile ((_ == Premise) <<< _.rule) st.rows of
          [] -> [ emptyRow ]
          rows -> rows
      in
        st { premisesInput = s, rows = newRows }
  UpdateConclusion s -> H.modify_ \st -> st { conclusion = s }
  ClearProof -> H.put $ initialState unit
  ShowHint -> do
    st <- H.get
    H.liftEffect $ Hint.showHint { premises: premises st.rows, conclusion: st.conclusion }
  AddBelow -> do
    { inFocus } <- H.get
    addRowBelow inFocus
  AddOutsideBox -> do
    { inFocus } <- H.get
    exitBox inFocus
  SetFocus i -> H.modify_ \st -> st { inFocus = i }
  where
  addRowBelow i = do
    H.modify_ \st ->
      st
        { rows =
          unsafePartial $ fromJust $ Array.insertAt (i + 1) emptyRow
            $ incrBoxEnds i st.rows
        }
    H.tell _symbolInput (2 * (i + 1)) SI.Focus -- Focus the newly added row

  -- | Deletes a row. If the row is the start of a box, delete the box.
  deleteRow i = do
    rowCount <- Array.length <$> H.gets _.rows
    -- Deleting the last row means no new rows can be added
    when (rowCount > 1) do
      H.modify_ \st ->
        st
          { rows = unsafePartial $ fromJust $ Array.deleteAt i $ decrBoxEnds i st.rows
          }
      H.tell _symbolInput (2 * (i - 1)) SI.Focus

  -- | Creates a new row directly below the current index. If the current
  -- | index is at the end of a box, the new row is created outside the box.
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
  -- | all boxes which end after the given index by one.
  incrBoxEnds :: Int -> Array ProofRow -> Array ProofRow
  incrBoxEnds = modifyBoxEnds 1

  -- | Takes an index and an array of rows. Decreases the ending position of
  -- | all boxes which end after the given index by one.
  decrBoxEnds :: Int -> Array ProofRow -> Array ProofRow
  decrBoxEnds = modifyBoxEnds (-1)

  -- | Takes an index, an offset and an array of rows. Shifts the end
  -- | of all boxes which end after the given index by the given offset.
  modifyBoxEnds :: Int -> Int -> Array ProofRow -> Array ProofRow
  modifyBoxEnds off i =
    map case _ of
      row@{ rule: Assumption { boxEndIdx } }
        | i <= boxEndIdx -> row { rule = Assumption { boxEndIdx: boxEndIdx + off } }
      row@{ rule: Fresh { boxEndIdx } }
        | i <= boxEndIdx -> row { rule = Fresh { boxEndIdx: boxEndIdx + off } }
      x -> x

  -- | Takes an index and an array of rows. Increases the ending position of
  -- | all boxes which end after the given index by one, except any box which
  -- | starts at the exception position.
  incrBoxEndsWithEx :: Int -> Int -> Array ProofRow -> Array ProofRow
  incrBoxEndsWithEx i e rs =
    map
      ( \(Tuple idx r) ->
          if idx == e then
            r
          else case r of
            row@{ rule: Assumption { boxEndIdx } }
              | (i <= boxEndIdx) -> row { rule = Assumption { boxEndIdx: boxEndIdx + 1 } }
            row@{ rule: Fresh { boxEndIdx } }
              | (i <= boxEndIdx) -> row { rule = Fresh { boxEndIdx: boxEndIdx + 1 } }
            x -> x
      )
      (enumerate rs)

  -- | Inclusive-exclusive interval of the rows that are currently being dragged.
  draggedRows :: H.HalogenM _ _ _ _ _ { start :: Int, end :: Int }
  draggedRows = do
    start <- unsafePartial $ fromJust <$> H.gets _.dragged
    rows <- H.gets _.rows
    let
      startRow = unsafePartial $ fromJust $ rows !! start
    let
      end = case startRow.rule of
        Assumption { boxEndIdx } -> boxEndIdx + 1
        Fresh { boxEndIdx } -> boxEndIdx + 1
        _ -> start + 1
    pure { start, end }

  isValidDropZone i = do
    maybeDragged <- H.gets _.dragged
    if isJust maybeDragged then
      (\{ start, end } -> not (start <= i && i < end)) <$> draggedRows
    else
      pure false

-- | Takes an index and a state and returns the starting position of the
-- | innermost box which contains the index.
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
  indexedBoxes st = Array.filter (\(Tuple _ r) -> isBox r) (enumerate st.rows)

  -- | Takes a row, assumed to be the start of a box, along with it's
  --   position in the proof. Returns a tuple with the limits of the box.
  boxLimits :: Tuple Int ProofRow -> Tuple Int Int
  boxLimits (Tuple i r) = case r.rule of
    (Assumption { boxEndIdx }) -> Tuple i boxEndIdx
    (Fresh { boxEndIdx }) -> Tuple i boxEndIdx
    _ -> unsafeCrashWith "Not a box."

  -- | Get a list of all boxes in a state as tuples.
  allBoxLimits :: State -> Array (Tuple Int Int)
  allBoxLimits st = map boxLimits $ indexedBoxes st

getAllEndings :: State -> Array Int
getAllEndings st = map rToE $ boxes st
  where
  rToE :: ProofRow -> Int
  rToE r = case r.rule of
    Assumption { boxEndIdx } -> boxEndIdx
    Fresh { boxEndIdx } -> boxEndIdx
    _ -> unsafeCrashWith "Row is not a box."

boxes :: State -> Array ProofRow
boxes st = Array.filter isBox st.rows

-- | Check if a row is the start of a box.
isBox :: ProofRow -> Boolean
isBox r = case r.rule of
  Assumption _ -> true
  Fresh _ -> true
  _ -> false

ruleFromString :: String -> Int -> Rule
ruleFromString s rowIdx
  | s == "Ass." || s == "as" = Assumption { boxEndIdx: rowIdx }
  | s == "pr" || s == "Premise" = Premise
  | s == "cp" || s == "co" || s == "Copy" = Rule "Copy"
  | s == "fr" || s == "Fresy" = Fresh { boxEndIdx: rowIdx }
  | otherwise = Rule s
