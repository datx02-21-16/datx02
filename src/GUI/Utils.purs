module GUI.Utils (makeRegex, digitRegex) where

import Prelude
import Data.String.Regex (Regex, regex)
import Data.Either (fromRight')
import Data.String.Regex.Flags (global)
import Partial.Unsafe (unsafeCrashWith)

makeRegex :: String -> Regex
makeRegex s = fromRight' (\_ -> unsafeCrashWith "Bad regex") $ regex s global

-- | Regex to make sure that a string contains only digits
digitRegex :: Regex
digitRegex = makeRegex $ "^[0-9]*$"
