module Util where

import Prelude

-- | The Haskell '>>' operator.
semicolon :: forall b a c. Bind b => b a -> b c -> b c
semicolon a c = do _ <- a
                   c
infixl 1 semicolon as >>
