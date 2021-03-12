module GUI.Utils (substituteAll) where

import Data.String.Regex (Regex, regex, replace)
import Data.Tuple (Tuple(..))
import Prelude
import Data.Either (fromRight)
import Data.String.Regex.Flags (global)
import GUI.Config.Syntax as Syntax
import Partial.Unsafe (unsafePartial)
import Data.Array (foldr)

substituteAll :: String -> String
substituteAll s = foldr (\t -> substitute t) s substitutions

substitute :: Tuple String String -> String -> String
substitute (Tuple from to) str = replace (makeRegex from) to str

substitutions :: Array (Tuple String String)
substitutions =
  [ Tuple ("""\W+""" <> Syntax.andText <> """\W+""") " ^ "
  , Tuple ("""\W+""" <> Syntax.orText <> """\W+""") " v "
  , Tuple ("""\W+""" <> Syntax.impText <> """\W+""") " -> "
  , Tuple ("""\W+""" <> Syntax.notText <> """\W+""") " ¬"
  -- Quantifiers
  , Tuple ("""\W+""" <> Syntax.forAllText <> """\W+""") "∀"
  , Tuple ("""\W+""" <> Syntax.existsText <> """\W+""") "∃"
  , Tuple ("""\W+""" <> Syntax.bottomText <> """\W+""") "⊥"
  ]

makeRegex :: String -> Regex
makeRegex s = unsafePartial (fromRight (regex s global))
