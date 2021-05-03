-- | This module generates hint for user-provided premises and
-- | conclusions, by first solving the sequent itself and then
-- | considering the order of →i/¬i in the proof.
module GUI.Hint (genHint, showHint) where

import Prelude
import Data.Tuple (Tuple(Tuple))
import Data.Array as Array
import Data.List (List(Nil), (:))
import Data.NonEmpty ((:|))
import Data.Foldable (foldl, foldM)
import Data.Traversable (sequence)
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

-- | Reconstructs the boxes from the given ND derivation.
-- |
-- | Takes note of whether the box is used as an argument to →i or ¬i.
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
              _ -> unsafeCrashWith "→i without prior assumption"
            NotIntro _ -> case parentBoxes of
              Box siblingBoxes : grandParentBoxes -> Box (Array.snoc siblingBoxes (Tuple (BCNotIntro row.formula) currentBox)) :| grandParentBoxes
              _ -> unsafeCrashWith "¬i without prior assumption"
            _ -> acc
        )
        (Box [] :| Nil)
        rows
  in
    case result of
      x :| Nil -> x
      _ -> unsafeCrashWith "Unclosed box"

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
            >>= \s -> case dfsForNotIntro innerBox of
                Right x -> Right $ s <> x
                Left x -> Left $ s <> x
      )
      ""
      innerBoxes

-- | Generates a hint for how to go about solving the given sequent.
genHint :: { premises :: Array String, conclusion :: String } -> String
genHint { premises, conclusion } =
  either identity identity do
    premises' <- note "Cannot read premises" $ hush $ sequence $ parseFormula <$> premises
    conclusion' <- note "Cannot read conclusion" $ hush $ parseFormula conclusion
    proof <- note "No solution found!" $ prove premises' conclusion'
    pure $ hintFromBoxes (constructBoxes proof)

-- | Shows a hint for solving the given sequent in a pop-up dialog.
showHint :: { premises :: Array String, conclusion :: String } -> Effect Unit
showHint sequent = window >>= Window.alert (genHint sequent)
