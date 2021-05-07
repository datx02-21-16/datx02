module FormulaOrVar
  ( FFC(FC, VC)
  , parseFFC
  ) where

import Prelude
import Control.Alternative ((<|>))
import Data.Either (Either)
import Text.Parsing.Parser (ParseError)
import Formula (Variable, Formula)
import Parser (parseFormula, parseVar)

data FFC
  = FC Formula
  | VC Variable

derive instance eqFFC :: Eq FFC

instance showFFC :: Show FFC where
  show (FC f) = "FC: " <> show f
  show (VC v) = "VC: " <> show v

parseFFC :: String -> Either ParseError FFC
parseFFC s = (VC <$> parseVar s) <|> (FC <$> parseFormula s)
