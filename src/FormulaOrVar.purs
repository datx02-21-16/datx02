module FormulaOrVar
  ( FFC(FC, VC)
  , parseFFC
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Either (Either)
import Text.Parsing.Parser (ParseError)
import Formula (Variable, Formula)
import Latex (class Latex, toLatex)
import Parser (parseFormula, parseVar)

data FFC
  = FC Formula
  | VC Variable

derive instance eqFFC :: Eq FFC

instance showFFC :: Show FFC where
  show (FC f) = "FC: " <> show f
  show (VC v) = "VC: " <> show v

instance latexFFC :: Latex FFC where
  toLatex (FC f) = toLatex f
  toLatex (VC v) = toLatex v

parseFFC :: String -> Either ParseError FFC
parseFFC s = (VC <$> parseVar s) <|> (FC <$> parseFormula s)
