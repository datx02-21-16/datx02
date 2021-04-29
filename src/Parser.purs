module Parser (formula, parseFormula) where

import Prelude
import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Data.Array as Array
import Data.String.CodePoints (codePointFromChar)
import Data.CodePoint.Unicode (isLower)
import Data.Either (Either)
import Data.Identity (Identity)
import Text.Parsing.Parser (Parser, ParserT, ParseError, runParser)
import Text.Parsing.Parser.Combinators (option, choice, chainl1, lookAhead, (<?>))
import Text.Parsing.Parser.String (char, oneOf, satisfy, eof)
import Text.Parsing.Parser.Token (GenLanguageDef(..), TokenParser, makeTokenParser, upper, letter)
import Text.Parsing.Parser.Expr (OperatorTable, Assoc(..), Operator(..), buildExprParser)
import Formula (Variable(..), Term(..), Formula(..))

token :: TokenParser
token = makeTokenParser languageDef
  where
  languageDef =
    LanguageDef
      { commentStart: ""
      , commentEnd: ""
      , commentLine: ""
      , nestedComments: false
      , identStart: letter <|> char '⊥'
      , identLetter: letter
      , opStart: oneOf [ '¬', '∧', '∨', '→', '∀', '∃' ]
      , opLetter: oneOf []
      , reservedNames: []
      , reservedOpNames: [ "¬", "∧", "∨", "→", "∀", "∃" ]
      , caseSensitive: true
      }

-- | Parse a lowercase letter.
lower :: forall m. Monad m => ParserT String m Char
lower = satisfy (isLower <<< codePointFromChar) <?> "lowercase letter"

variableSymbol :: Parser String String
variableSymbol = lookAhead lower *> token.identifier

variable :: Parser String Variable
variable = Variable <$> variableSymbol <?> "variable"

argumentList :: forall a. Parser String a -> Parser String (Array a)
argumentList p = Array.fromFoldable <$> token.parens (token.commaSep p)

term :: Parser String Term
term = do
  symbol <- variableSymbol
  -- Constants require empty argument list ("c()") to disambiguate from variables
  option (Var $ Variable symbol) (App symbol <$> argumentList term)

-- | Parse a single predicate variable such as P(x).
predicate :: Parser String Formula
predicate =
  let
    predicateSymbol =
      token.symbol "=" <|> lookAhead upper *> token.identifier
        <|> token.symbol "⊥"
        <?> "uppercase proposition/predicate symbol"

    -- The equality predicate is usually written using infix notation
    equality =
      do
        x <- term
        _ <- token.symbol "="
        y <- term
        pure $ Predicate "=" [ x, y ]
        <?> "equality"
  in
    equality
      <|> do
          symbol <- predicateSymbol
          -- For nullary predicates the argument list is optional
          args <- option [] (argumentList term)
          pure $ Predicate symbol args

-- | Parse a single formula.
formula :: Parser String Formula
formula = fix allFormulas
  where
  forallParser = Forall <$> (token.reservedOp "∀" *> variable)

  existsParser = Exists <$> (token.reservedOp "∃" *> variable)

  -- Required because buildExprParser does not allow multiple prefix
  -- operators of the same precedence (e.g. ¬¬A).
  chained p = chainl1 p $ pure (<<<)

  opTable :: OperatorTable Identity String Formula
  opTable =
    [ [ Prefix $ chained
          $ choice
              [ token.reservedOp "¬" $> Not
              , forallParser
              , existsParser
              ]
      ]
    , [ Infix (token.reservedOp "∧" $> And) AssocLeft ]
    , [ Infix (token.reservedOp "∨" $> Or) AssocLeft ]
    , [ Infix (token.reservedOp "→" $> Implies) AssocRight ]
    ]

  allFormulas p =
    let
      singleFormula = token.parens p <|> predicate
    in
      buildExprParser opTable singleFormula

-- | Returns the result of parsing a formula from the specified string.
parseFormula :: String -> Either ParseError Formula
-- The reservedOp parsers are a little stingy in that they appear to
-- not treat EOF as a symbol boundary, so append a single whitespace
-- to improve error messages.
parseFormula = flip runParser (token.whiteSpace *> formula <* eof) <<< (_ <> " ")
