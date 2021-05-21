module Parser (formula, parseFormula, parsePremises, parseVar) where

import Prelude
import Control.Alternative ((<|>))
import Control.Lazy (defer)
import Data.Array as Array
import Data.List as List
import Data.List ((:), many)
import Data.List.NonEmpty (NonEmptyList)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Data.CodePoint.Unicode (isLower)
import Data.Maybe (fromJust)
import Data.Either (Either, fromRight')
import Data.Identity (Identity)
import Data.String (Pattern(Pattern))
import Data.String as String
import Control.Monad.State.Class (gets)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Text.Parsing.Parser (Parser, ParserT, ParseError, ParseState(ParseState), runParser, fail)
import Text.Parsing.Parser.Combinators ((<?>), option, try, sepBy1, lookAhead, notFollowedBy)
import Text.Parsing.Parser.String (anyChar, char, oneOf, satisfy, eof)
import Text.Parsing.Parser.Token (GenLanguageDef(..), TokenParser, makeTokenParser, digit, upper, letter, alphaNum)
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
      , identStart: letter
      , identLetter: alphaNum
      , opStart: oneOf [ '¬', '∧', '∨', '→', '∀', '∃' ]
      , opLetter: fail "No alternative"
      , reservedNames: []
      , reservedOpNames: [ "¬", "∧", "∨", "→", "∀", "∃" ]
      , caseSensitive: true
      }

-- | Parse a lowercase letter.
lower :: forall m. Monad m => ParserT String m Char
lower = satisfy (isLower <<< codePointFromChar) <?> "lowercase letter"

variableSymbol :: Parser String String
variableSymbol =
  token.lexeme do
    start <- lower
    rest <- many (lower <|> digit)
    pure $ fromCharArray $ List.toUnfoldable (start : rest)

variable :: Parser String Variable
variable = Variable <$> variableSymbol <?> "variable"

term :: Parser String Term
term =
  do
    symbol <- variableSymbol
    -- Constants require empty argument list ("c()") to disambiguate from variables
    option (Var $ Variable symbol)
      (App symbol <<< List.toUnfoldable <$> token.parens (token.commaSep term))
    <?> "term"

-- | Parse a single atomic proposition such as p.
proposition :: Parser String Formula
proposition = flip Predicate [] <$> (token.symbol "⊥" <|> token.identifier) <?> "proposition"

-- | Parse a single predicate variable such as P(x).
predicate :: Parser String Formula
predicate =
  let
    predicateSymbol =
      token.symbol "=" <|> lookAhead upper *> token.identifier
        <?> "uppercase predicate symbol"

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
          args <- Array.fromFoldable <$> token.parens (token.commaSep1 term)
          pure $ Predicate symbol args

-- | Parse a single formula.
formula :: Parser String Formula
formula = compoundFormula propOrPred
  where
  propOrPred =
    try (proposition <* notFollowedBy (void $ oneOf [ '(', '=' ])) <|> predicate
      <?> "proposition/predicate"

  forallParser = Forall <$> (token.reservedOp "∀" *> variable)

  existsParser = Exists <$> (token.reservedOp "∃" *> variable)

  opTable :: OperatorTable Identity String Formula
  opTable =
    [ [ Infix (token.reservedOp "∧" $> And) AssocLeft ]
    , [ Infix (token.reservedOp "∨" $> Or) AssocLeft ]
    , [ Infix (token.reservedOp "→" $> Implies) AssocRight ]
    ]

  compoundFormula atom =
    let
      singleFormula atom' =
        token.parens (defer \_ -> compoundFormula atom')
          <|> (token.reservedOp "¬" $> Not <*> defer \_ -> singleFormula atom')
          <|> (forallParser <*> defer \_ -> singleFormula predicate)
          <|> (existsParser <*> defer \_ -> singleFormula predicate)
          <|> atom'
    in
      buildExprParser opTable (singleFormula atom) <?> "formula"

-- | Returns the result of parsing the specified string as a formula.
parseFormula :: String -> Either ParseError Formula
-- The reservedOp parsers are a little stingy in that they appear to
-- not treat EOF as a symbol boundary, so append a single whitespace
-- to improve error messages.
parseFormula = flip runParser (token.whiteSpace *> formula <* eof) <<< (_ <> " ")

parsedString :: forall m a. Monad m => ParserT String m a -> ParserT String m String
parsedString p = do
  initial <- getInput
  _ <- p
  remaining <- getInput
  pure $ unsafePartial $ fromJust $ String.stripSuffix (Pattern remaining) initial
  where
  getInput :: forall s. ParserT s m s
  getInput = gets \(ParseState input _ _) -> input

-- | Parses a comma-separated premises string.
-- |
-- | If a premise fails to parse it is replaced with the remaining
-- | input.
parsePremises :: String -> NonEmptyList String
parsePremises s =
  let
    result = runParser s $ token.whiteSpace *> sepBy1 premise (char ',' <* token.whiteSpace)

    premise = parsedString $ void (try $ formula <* lookAhead (char ',')) <|> void (many anyChar)
  in
    fromRight' (\_ -> unsafeCrashWith "unreachable (parser always succeeds)") result

parseVar :: String -> Either ParseError Variable
parseVar = flip runParser $ token.whiteSpace *> variable <* eof
