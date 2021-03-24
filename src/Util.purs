module Util where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe)
import Partial.Unsafe (unsafePartial)
import Data.Traversable (mapAccumL, class Traversable)
import Debug (class DebugWarning, trace)

findLast :: forall a. (a -> Boolean) -> Array a -> Maybe a
findLast f xs = (\i -> unsafePartial $ Array.unsafeIndex xs i)
                <$> Array.findLastIndex f xs

traceShowId :: forall a. DebugWarning => Show a => a -> a
traceShowId x = trace (show x) \_ -> x
