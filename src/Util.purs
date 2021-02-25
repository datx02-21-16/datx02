module Util where

import Prelude
import Data.Traversable (mapAccumL, class Traversable)
import Debug (class DebugWarning, trace)

-- | The Haskell '>>' operator.
semicolon :: forall b a c. Bind b => b a -> b c -> b c
semicolon a c = do _ <- a
                   c
infixl 1 semicolon as >>

enumerate :: forall a f. Traversable f => f a -> f { i :: Int, x :: a }
enumerate xs = value
  where { accum, value } = mapAccumL (\s x -> { accum: s + 1, value: { i: s, x } }) 0 xs

traceShowId :: forall a. DebugWarning => Show a => a -> a
traceShowId x = trace (show x) \_ -> x
