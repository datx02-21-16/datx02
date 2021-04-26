module GUI.Hint where

import Prelude
import Data.Tuple (Tuple(Tuple))
import Data.Array as Array
import Data.List (List(Nil), (:))
import Data.NonEmpty ((:|))
import Data.Foldable (foldl, foldM)
import Data.Traversable (sequence)
import Data.Maybe (fromMaybe)
import Data.Either (Either(Left, Right), either, note, hush)
import Partial.Unsafe (unsafeCrashWith)
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window as Window
import Formula (Formula(Implies, Not))
import NdAlg (Rule(..), prove)
import Parser (parseFormula)

data BoxCloser
  = BCImpliesIntro Formula
  | BCNotIntro Formula

data Box
  = Box (Array (Tuple BoxCloser Box))

constructBoxes ::
  Array { formula :: Formula, rule :: Rule } ->
  Box
constructBoxes rows =
  let
    result =
      foldl
        ( \acc@(currentBox :| parentBoxes) row -> case row.rule of
            Assumption -> Box [] :| currentBox : parentBoxes
            ImpliesIntro _ -> case parentBoxes of
              Box siblingBoxes : grandParentBoxes -> Box (Array.snoc siblingBoxes (Tuple (BCImpliesIntro row.formula) currentBox)) :| grandParentBoxes
              _ -> unsafeCrashWith "unreachable"
            NotIntro _ -> case parentBoxes of
              Box siblingBoxes : grandParentBoxes -> Box (Array.snoc siblingBoxes (Tuple (BCNotIntro row.formula) currentBox)) :| grandParentBoxes
              _ -> unsafeCrashWith "unreachable"
            _ -> acc
        )
        (Box [] :| Nil)
        rows
  in
    case result of
      x :| Nil -> x
      _ -> unsafeCrashWith "unreachable"

hintFromBoxes :: Box -> String
hintFromBoxes (Box []) = "Use direct reasoning"

hintFromBoxes box =
  "Try starting with:\n"
    <> either identity identity (dfsForNotIntro box)
  where
  dfsForNotIntro :: Box -> Either String String
  dfsForNotIntro (Box innerBoxes) =
    foldM
      ( \prevStr (Tuple boxCloser innerBox) ->
          case boxCloser of
            BCImpliesIntro (Implies f _) -> Right $ prevStr <> "\n Assume " <> show f
            BCImpliesIntro _ -> unsafeCrashWith "Cannot do →i on anything but Implies"
            BCNotIntro (Not f) -> Left $ prevStr <> "\n PBC by assuming " <> show f
            BCNotIntro _ -> unsafeCrashWith "Cannot do ¬i on anything but Not"
            <> dfsForNotIntro innerBox
      )
      ""
      innerBoxes

showHint :: { premises :: Array String, conclusion :: String } -> Effect Unit
showHint { premises, conclusion } = do
  let
    hint = do
      premises' <- note "Cannot read premises" $ hush $ sequence $ parseFormula <$> premises
      conclusion' <- note "Cannot read conclusion" $ hush $ parseFormula conclusion
      proof <- note "No solution found!" $ prove premises' conclusion'
      let
        boxes = constructBoxes proof
      pure $ hintFromBoxes boxes
  window >>= Window.alert (either identity identity hint)
