module GUI.Utils (makeRegex) where

import Prelude
import Data.String.Regex (Regex, regex)
import Data.Tuple (Tuple(..))
import Data.Either (fromRight')
import Data.String.Regex.Flags (global)
import GUI.Config.Syntax as Syntax
import Partial.Unsafe (unsafeCrashWith)

makeRegex :: String -> Regex
makeRegex s = fromRight' (\_ -> unsafeCrashWith "Bad regex") $ regex s global
