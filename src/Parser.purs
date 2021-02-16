module Parser where

import Prelude

import Control.Lazy (fix)
import Data.Identity (Identity)
import Data.Char.Unicode (isLower)
import Data.String.CodeUnits (singleton)
import Data.List (many)
import Data.Array as Array
import Data.Either (Either)
import Util ((>>))

import Text.Parsing.Parser.Token (LanguageDef, GenLanguageDef(..), TokenParser, makeTokenParser,
                                  upper, letter, space)
import Text.Parsing.Parser.Combinators (lookAhead, sepBy1)
import Text.Parsing.Parser.String (oneOf, satisfy, char)
import Text.Parsing.Parser.Expr (OperatorTable, Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser (Parser, ParseError, runParser)
import Control.Alternative ((<|>))

import Formula (Variable(..), Term(..), Formula(..))

languageDef :: LanguageDef
languageDef = LanguageDef
              { commentStart: ""
              , commentEnd: ""
              , commentLine: ""
              , nestedComments: false
              , identStart: letter
              , identLetter: letter
              , opStart: oneOf ['¬', '∧', '∨', '→', '∀', '∃']
              , opLetter: oneOf []
              , reservedNames: []
              , reservedOpNames: ["¬", "∧", "∨", "→", "∀", "∃"]
              , caseSensitive: true
              }

token :: TokenParser
token = makeTokenParser languageDef
identifier :: Parser String String
identifier = token.identifier
parens :: forall a. Parser String a -> Parser String a
parens = token.parens

lower :: Parser String Char
lower = satisfy isLower

variable :: Parser String Variable
variable = Variable <$> ((lookAhead (singleton <$> lower)) >> identifier)

term :: Parser String Term
term = (Var <$> variable)

predicate :: Parser String Formula
predicate = let
  predicateSymbol = (lookAhead upper) >> identifier
  in do
    symbol <- predicateSymbol
    args <- (Array.fromFoldable <$> parens (sepBy1 term (many space >> char ',' >> many space)))
            <|> pure []
    pure $ Predicate symbol args

formula :: Parser String Formula
formula = fix allFormulas
  where
    forallParser :: Parser String (Formula -> Formula)
    forallParser = Forall <$> (token.reservedOp "∀" >> variable)

    existsParser :: Parser String (Formula -> Formula)
    existsParser = Exists <$> (token.reservedOp "∃" >> variable)

    opTable :: OperatorTable Identity String Formula
    opTable =
      [ [ Prefix (token.reservedOp "¬" $> Not)
        , Prefix forallParser
        , Prefix existsParser ]
      , [ Infix (token.reservedOp "∧" $> And) AssocLeft
        , Infix (token.reservedOp "∨" $> Or) AssocLeft
        , Infix (token.reservedOp "→" $> Implies) AssocRight ] ]

    allFormulas p = let
      singleFormula = parens p <|> predicate
      in buildExprParser opTable singleFormula

parseFormula :: String -> Either ParseError Formula
parseFormula = flip runParser $ formula
