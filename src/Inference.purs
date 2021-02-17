module Inference where

import Prelude
import Data.Either (Either)
import Control.Monad.Except (Except, throwError, runExcept)
import Formula (Formula(..))

{- | Different kind of errors that can be produced when using these
'inference rules' to produce formulas. -}
data NDErrors = NotAConjunction  Formula
              | NotADisjunction  Formula
              | NotAnImplication Formula
              | BadOrElimination Formula -- left operand of the or formula
                                 Formula -- result of trying to eliminate the left operand
                                 Formula -- right operand of the or formula
                                 Formula -- result of trying to eliminate the right operand
              | BadImplElimination Formula -- first operand of the implication
                                   Formula -- second operand of the implication
                                   Formula -- the formula we attempted to eliminate the implication with

derive instance eqNDErrors :: Eq NDErrors

instance showNDErrors :: Show NDErrors where
  show (NotAConjunction e)            = "not a conjunction: " <> show e
  show (NotADisjunction e)            = "not a disjunction: " <> show e
  show (NotAnImplication e)           = "not an implication: " <> show e
  show (BadOrElimination e1 e2 e3 e4) = "bad or elimination: \n" <>
                                        "  " <> show e1 <> " → " <> show e2 <> "\n" <>
                                        "  " <> show e3 <> " → " <> show e4 <> "\n" <>
                                        "should be equal: " <> show e2 <> " " <> show e4
  show (BadImplElimination e1 e2 e3)  = "bad implication elimination: \n" <>
                                        " " <> show e1 <> " implies " <> show e2 <> "\n" <>
                                        "should be equal: " <> show e1 <> " and " <> show e3

-- | Natural deduction monad
type ND a = Except NDErrors a

-- | Run a ND computation
runND :: ND Formula -> Either NDErrors Formula
runND = runExcept

-- | And elimination-L. Produces an error if the input formula is not a conjunction.
andElimL :: Formula -> ND Formula
andElimL (And l _) = pure l
andElimL e         = throwError $ NotAConjunction e

-- | And elimination-R. Produces an error if the input formula is not a conjunction.
andElimR :: Formula -> ND Formula
andElimR (And _ r) = pure r
andElimR e         = throwError $ NotAConjunction e

-- | And introduction.
andIntro :: Formula -> Formula -> ND Formula
andIntro l r = pure $ And l r

{- | Or elimination. If we have A \/ B and two functions that go from A to C, and
B to C, we can deduce C. Produces an error if the first input argument is not a
disjunction, or if the two functions don't produce equal results. -}
orElim :: Formula -> (Formula -> ND Formula) -> (Formula -> ND Formula) -> ND Formula
orElim (Or l r) f g = do
    r1 <- f l      -- try to eliminate left operand
    r2 <- g r      -- try to eliminate right operand
    if eq r1 r2    -- did they produce equal formulas?
      then pure r1 -- then return that formula
      else throwError $ BadOrElimination  l r1 r r2
orElim e _ _ = throwError $ NotADisjunction e

{- or intro not possible in a straightforward manner.
Need something of the type orIntroL :: Formula -> ND Formula, but
we don't know with the second component of the or formula should be.

orIntroL :: Formula -> ND Formula
orIntroL l = Or l ?

orIntroR :: Formula -> ND Formula
orIntroR r = Or ? r

-}

{- | Implication introduction. If we say what we are assuming and supply a
function that when applied to what we are assuming produce some formula, we
can deduce that the assumed formula implies the derived formula. -}
implIntro :: Formula -> (Formula -> ND Formula) -> ND Formula
implIntro f box = box f

{-} | If we have an implicatino such as A -> B, and an A, we can deduce B. Produces an error
if the first input argument is not an implication, or if the second input argument is
not the same as the first operand of the implication. -}
implElim :: Formula -> Formula -> ND Formula
implElim (Implies a b) ma = if a == ma     -- Is the elimination valid?
                              then pure b  -- return the implied formula
                              else throwError $ BadImplElimination a b ma
implElim e _ = throwError $ NotAnImplication e