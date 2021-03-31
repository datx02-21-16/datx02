module Util where

import Prelude
import Data.Maybe (Maybe)
import Data.Array as Array
import Data.Array (slice)
import Partial.Unsafe (unsafePartial)
import Data.Tuple (Tuple(Tuple))
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Debug (class DebugWarning, trace)

enumerate :: forall i f a. FunctorWithIndex i f => f a -> f (Tuple i a)
enumerate = mapWithIndex Tuple

-- | Move a part of an array within itself.
moveWithin :: forall a. Int -> Int -> Int -> Array a -> Array a
moveWithin target start end =
  if target == start then
    identity
  else
    if target < start then
      go target start end
    else
      go start end target
  where
  go i j k xs =
    Array.concat
      [ Array.take i xs
      , slice j k xs
      , slice i j xs
      , Array.drop k xs
      ]

findLast :: forall a. (a -> Boolean) -> Array a -> Maybe a
findLast f xs =
  (\i -> unsafePartial $ Array.unsafeIndex xs i)
    <$> Array.findLastIndex f xs

traceShowId :: forall a. DebugWarning => Show a => a -> a
traceShowId x = trace (show x) \_ -> x
