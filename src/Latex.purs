module Latex (class Latex, toLatex) where

-- | A class for types that can be converted to LaTeX.
class Latex a where
  toLatex :: a -> String
