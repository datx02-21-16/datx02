module Inference where

import Prelude

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Either (Either)
import Formula (Formula(..), Term(..), Variable(..), containsTerm, substitution)

{- | Different kind of errors that can be produced when using these
'inference rules' to produce formulas. -}
data NDErrors = NotAConjunction  Formula
              | NotADisjunction  Formula
              | NotAnImplication Formula
              | NotAModusTollens Formula Formula
              | NotADoubleNeg    Formula
              | NotANegElim Formula Formula
              | NotAForall Formula
              | NotPBC Formula Formula
              | BadOrElimination Formula -- left operand of the or formula
                                 Formula -- result of trying to eliminate the left operand
                                 Formula -- right operand of the or formula
                                 Formula -- result of trying to eliminate the right operand
              | BadImplElimination Formula -- first operand of the implication
                                   Formula -- second operand of the implication
                                   Formula -- the formula we attempted to eliminate the implication with
              | BadModusTollens Formula
                                Formula
                                Formula
              | BadNegElim Formula Formula
              | BadExistsElim Formula Term
              | BadExistsIntro Formula Term



derive instance eqNDErrors :: Eq NDErrors

instance showNDErrors :: Show NDErrors where
  show (NotAConjunction e)            = "not a conjunction: " <> show e
  show (NotADisjunction e)            = "not a disjunction: " <> show e
  show (NotAnImplication e)           = "not an implication: " <> show e
  show (NotAModusTollens e1 e2)       = "not a modus tollens: \n" <> 
                                        "Got the following: " <> "Formula1 = " <> show e1 <> ", Formula2 = " <> show e2
  show (NotANegElim e1 e2)            = "not a negation elimination: \n" <>
                                        "Got the following: " <> "Formula1 = " <> show e1 <> ", Formula2 = " <> show e2 
  show (NotAForall f)                 = "Not a forall: \n" <> show f
  show (NotADoubleNeg e)              = "No double negation on formula: " <> show e
  show (NotPBC f1 f2)                 = "Not a proof by contradiction: \n" <>
                                        "¬" <> show f1 <> " should lead to ⊥, but lead to " <> show f2
  show (BadOrElimination e1 e2 e3 e4) = "bad or elimination: \n" <>
                                        "  " <> show e1 <> " → " <> show e2 <> "\n" <>
                                        "  " <> show e3 <> " → " <> show e4 <> "\n" <>
                                        "should be equal: " <> show e2 <> " " <> show e4
  show (BadImplElimination e1 e2 e3)  = "bad implication elimination: \n" <>
                                        " " <> show e1 <> " implies " <> show e2 <> "\n" <>
                                        "should be equal: " <> show e1 <> " and " <> show e3
  show (BadModusTollens e1 e2 e3)     = "bad modus tollens application: \n" <> " " <> "Formula1 = " <> show e1 <> " implies " <> show e2 <> " , Formula2 = " <> show e3 <> "\n" <> 
                                        "should be: " <>  "Formula1 = " <> show e1 <> " implies " <> show e2 <> ", Formula2 = " <> show (Not e2)
  show (BadNegElim e1 e2)             = "bad negation elimination application: \n" <> " " <> "Formula1 = " <> show  e1   <>  " Formula2 = " <> show e2
  show (BadExistsElim f t)            = "bad exists elimination: \n" <>
                                        "formula " <> show f <> " contains term " <> show t
  show (BadExistsIntro f t)           = "bad exists introduction: \n" <>
                                        "formula " <> show f <> " does not contain term " <> show t

-- | Natural deduction monad
type ND a = StateT Int (Except NDErrors) a


-- | Run a ND computation
runND :: ND Formula -> Either NDErrors Formula
runND ma = runExcept (evalStateT ma 0)

fresh :: ND Variable
fresh = do
  st <- get
  put $ st + 1
  pure $ Variable $ "fresh" <> (show st)

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
implIntro assumption box = do
    implied <- box assumption
    pure $ Implies assumption implied

{-} | If we have an implicatino such as A -> B, and an A, we can deduce B. Produces an error
if the first input argument is not an implication, or if the second input argument is
not the same as the first operand of the implication. -}
implElim :: Formula -> Formula -> ND Formula
implElim (Implies a b) ma = if a == ma     -- Is the elimination valid?
                              then pure b  -- return the implied formula
                              else throwError $ BadImplElimination a b ma
implElim e _ = throwError $ NotAnImplication e

closeBox :: Formula -> ND Formula
closeBox = pure

-- notElim is the rule ¬e

notElim :: Formula -> Formula -> ND Formula 
notElim phi (Not phi2) = if phi == phi2                                  -- Given Φ1 and ¬Φ2 , check that Φ1 == Φ2
                            then pure (Bottom)                           -- return the deduced formula ⊥ 
                            else throwError (BadNegElim phi (Not phi2))  -- if Φ1 != Φ2 throw specific error.
notElim e1 e2          = throwError (NotANegElim e1 e2)                  -- If input formulas are not in the form Φ and ¬Φ then throw specific error.


-- doubleNotElim is the rule ¬¬e

doubleNotElim :: Formula -> ND Formula 
doubleNotElim (Not (Not phi)) = pure phi       -- Given a formula ¬¬Φ , output Φ
doubleNotElim e = throwError (NotADoubleNeg e) -- If a formula doesnt have the form ¬¬Φ, output error. 



--modusTollens is the macro rule MT. 

modusTollens :: Formula -> Formula -> ND Formula
modusTollens (Implies phi psi) (Not psi2) = if psi == psi2                                  -- if we got Φ -> Ψ1 and ¬Ψ2, check that Ψ1 == Ψ2
                                             then pure (Not phi)                            -- return the deduced formula ¬Φ
                                             else throwError (BadModusTollens phi psi psi2) -- if Ψ1 != Ψ2 , throw specific error.
modusTollens e1 e2                        =  throwError (NotAModusTollens e1 e2)            -- If input formulas are not in the form Φ -> Ψ1 and ¬Ψ2 then throw specific error. 



--doubleNotIntro is the ¬¬i rule. Given a formula Φ as input, return ¬¬Φ.

doubleNotIntro :: Formula -> ND Formula 
doubleNotIntro phi = pure (Not(Not phi))

{-

TODO propositional logic rules : pbc and lem.

Not sure with pbc, but i was thinking about some pseudocode for lem atleast:

Lem doesnt have any explicit inputs to output a formula. However, the user will supply a formula
using the program in the gui somehow. A possible pseudocode (with tweaking in later stages ofcourse) could look like: 

func lem = do
phi  <- getInputFromUser        (From the gui)
phi2 <- getInputFromUser        (From the gui)
check isFormulaWellFormed phi  
check isFormulaWellFormed phi2
check that phi2 is a negated formula, thus has the form (Not (phi3)), if not throw error.
check phi3 == phi  , if not throw error
If all stages above are true, then output pure (Or phi phi2) else throw some kind of error.
 

-}

lem :: Formula -> ND Formula
lem f = pure $ Or f (Not f)

pbc :: Formula -> (Formula -> ND Formula) -> ND Formula
pbc f box = do
    f' <- box (Not f)
    if f' == Bottom
      then pure f
      else throwError $ NotPBC f f'

{-

Todo predicate logic rules: 

We need to somehow be able to provide a term that is going to be substituted in the variable

forallElim :: Formula -> ND Formula
forallElim (Forall var psi) = substitution psi ? var         where ? = some term
forallElim _          -> throwError 

...... 

-}

forallElim :: Formula -> ND Formula
forallElim (Forall var psi) = do
    var' <- fresh
    pure $ substitution psi (Var var') var
forallElim f          = throwError $ NotAForall f

{-

-- Might want something like this though, not sure.
-- On page 111 in the logic book there is an example with forall elimination. It is
-- very important in that proof that the forall elimination introduces a variable x0,
-- as it can then be used with the other things in the proof. The above version
-- will just introduce a random variable.

forallElim :: Formula -> Term -> ND Formula
forallElim (Forall var psi) var' = pure $ substitution psi var' var
forallElim f _                   = throwError $ NotAForall f
-}

forallIntro :: Variable -> Variable -> ND Formula -> ND Formula
forallIntro x0 x mf = do
    f <- mf
    pure $ substitution f (Var x) x0

{-

want to prove: A

forall x . P(x)        -- premise
(forall y . P(y)) -> A -- premise
[ x0                   -- assumption
  P(x0)                -- forallelim
]
forall y . P(y)        -- forall intro
A                      -- Impl elim

In this proof the choice of y in the forall introduction is necessary to be
able to do the implication elimination. This is why we make forallIntro take two
arguments.

-}

existsElim :: Formula -> Variable -> (Formula -> ND Formula) -> ND Formula
existsElim (Exists x f) x0 box = do
    f' <- box $ substitution f (Var x0) x
    if containsTerm f' (Var x0)
      then throwError $ BadExistsElim f' (Var x0)
      else pure f'
existsElim _ _ _ = pure Bottom

existsIntro :: Formula -> Variable -> Variable -> ND Formula
existsIntro f t x =  if containsTerm f (Var t)
       then pure $ Exists x (substitution f (Var x) t)
       else throwError $ BadExistsIntro f (Var t)

{-

P(t)            -- premise
Exists x . P(x) -- exists intro
-}