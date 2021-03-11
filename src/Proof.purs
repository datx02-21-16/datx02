module Proof where 

import Data.Either
import Data.Maybe
import Data.String
import Formula
import InferenceAlt
import Prelude

import Data.Array (deleteAt, head, index, insert, insertAt, length, tail, range)
import Data.Int.Bits ((.&.))
import Data.Maybe (fromJust)
import Partial.Unsafe as Partial


--data Proof = Proof {lines :: Array Row , assumptions :: Array Box , proofNumber :: Int}
--data Row = Row {rowNumber :: Int , formula :: Maybe Formula , rule :: Maybe Rule , applyRuleOn :: Array Int}
--data Box = Box {lines :: Array Row , boxNumber :: Int , open :: Boolean}

--data RuleApp = {rule :: Rule , formulas :: Array Formula}

1. f1                        nr 1
2. f2                        nr 1
3. f3                        nr 1
4  |                         nr 0

newtype Proof :: Array ProofElement
data ProofElement :: BoxOpen | BoxClose | RowElem Row
data Row = Row {formula :: Maybe Formula, rule:: Maybe Rule, args :: Array Int}
data ValidationEnv = ValidationEnv {provenElements :: Array Row, nextRow :: Int, scopes :: Array (Array Int), termScopes :: Array (Array Term)}

newEnv :: ValidationEnv
newEnv = ValidationEnv {provenElements = [], nextRow = 1, rowScopes [[], boxScopes :: Array Array(Int, Int)}




-- provenElemements = [RowElement (Row {formula = P rule = Assume, args = []},
 --                    RowElement (Row {formula = Q rule = , args = []}
   --                  RowElement (Row {formula = Implies P Q rule = ImplIntro, args = []} ]
-- scopes = [[1],[2]]
-- boxScopes[[],[(4,5)]]
1 p    Premise
2 p -> q Premise
3 q ImplElim
2 q
3 P -> P


1 P         Premise            -- scopes = [[]], boxscopes = [[]], nextRow = 1
2 |P        Assume             -- scopes = [[1],[]], boxscopes = [[],[]], nextRow = 2
3 |Q                           -- scopes = [[1],[2]], boxscopes = [[],[]], nextRow = 3
4 ||                           -- scopes = [[1],[2,3],[]], boxscopes = [[],[],[]], nextRow = 4
5 ||                           -- scopes = [[1],[2,3],[4]], boxscopes = [[],[],[]], nextRow = 5
6 |R                           -- scopes = [[1],[2,3]], boxscopes = [[],[(4,5)]]
7 P->R     ImplIntro           -- scopes = [[1]], boxscopes = [[(2,6)]]
8 |P                           -- scopes = [[1,7],[]], boxscopes = [[(2,6)],[]]
9 |                            -- scopes = [[1,7],[8]], boxscopes = [[(2,6)],[]]
10||                           -- scopes = [[1,7],[8,9],[]], boxscopes = [[(2,6)],[],[]]
11|| R     ImplElim 8,2-6      -- scopes = [[1,7],[8,9],[10]], boxscopes = [[(2,6)],[],[]]
12|                            -- scopes = [[1,7],[8,9]], boxscopes = [[(2,6)],[(10,11)]]
13                             -- scopes = [[1,7]], boxscopes = [[(2,6),(8,12)]]


E x P(x)

| a    P(a)               Assume 
|   ---
|  ----
|     P(a)
   For all x P(x)

4 P -> Q    ImplIntro
5 Q         ImplElim 1,4


{--

Prove P and Q, Q -> R, R -> S |- S      (without assumptions)

Proof [
  RowElement (Row {formula = And P Q, rule = Premise, args = []},   1
  RowElement (Row {formula = Imp Q R, rule = Premise, args = []},   2
  RowElement (Row {formula = Imp R S, rule = Premise, args = []},   3
  RowElement (Row {formula = Q, rule = AndElimE2, args = [1]},      4
  RowElement (Row {formula = R, rule = ImplElim, args = [2,4]},     5
  RowElement (Row {formula = S, rule = ImplElim, args = [3,5]}      6
]

--}
validateProof :: Proof -> State ValidationEnv Boolean

evalRule :: RuleApp -> Either String Formula
evalRule ra = case ra.rule of
  AndElimE1 -> if (length ra.args eq 1) then
    case (head args) of
      (And f1 f2) -> Right f1
      _ -> Left ("Wrong kind of formula")                            
    else (Left "Wrong number of arguments")
  AndElimE2 -> if (length ra.args eq 1) then
    case (head args) of
      (And f1 f2) -> Right f2
      _ -> Left ("Wrong kind of formula")                            
    else (Left "Wrong number of arguments")

validateProof :: Proof -> Boolean

validateElement :: ProofElement -> Either String Formula
validateELement pe = case pe of
  (BoxElement p) -> validateProof p
  (FormulaElement fe) -> case evalRule of
                          (Right f) ->  if ((fromJust fe.formula) eq f)
                                          then Right f
                                          else (Left "Rule does not produce that formula.")
                          (Left em) -> (Left ("Invalid rule application: " ++ em))


Proof 

1. Formula Rule  applyOnLine



{-
Proof

applyAssumption :: Int -> Rule -> Proof -> Either String Proof 
applyAssumption line (Assume) (Proof {lines : x , assumptions : listAssumptions}) = 3

1. P and Q    Premise  []              
2. P          AndElim E1 1 
3. | f1
4. | f2
5. | f3
6.   
-}


instance showProof :: Show Proof where
  show (Proof {lines : rows , assumptions : a})  = case rows , a of 
                                [] , [] -> "Empty proof"
                                xs , ys -> "\n" <> joinWith "\n" (map show xs) <> "\n" <> "\n" <> joinWith "\n" (map show ys)

instance showRow :: Show Row where 
    show (Row {rowNumber : i , formula : Just (f) , rule : Just (r) , applyRuleOn : i2}) = "Row: " <> show i <> "     Formula: " <> show f <> " " <> "      Rule: " <> show r <> "     Line: "    <> show i2 
    show _ = ""

instance showBox :: Show Box where 
   show (Box {lines : rows , boxNumber : i}) = case rows of 
                                                  [] -> ""
                                                  xs -> "Assumption " <> show i <> ":   " <> joinWith "\n" (map show xs) 






evalUnaryRuleOnProof :: Rule -> Proof -> Int -> Either String Proof
evalUnaryRuleOnProof rule (Proof {lines : listRows , assumptions : listAssumptions , proofNumber : pN }) line  = 
         let 
             totalRows = length listRows
         in case rule , index listRows (line-1) of
                (AndElimE1) , (Just (Row {formula : f })) -> case Partial.unsafePartial (fromJust (f)) of 
                                                                                    (And left _ ) -> helperFunc listRows totalRows left rule line
                                                                                    _ -> printError rule f

                (AndElimE2) , (Just (Row {formula : f })) -> case Partial.unsafePartial (fromJust (f)) of 
                                                                                    (And _ right ) -> helperFunc listRows totalRows right rule line
                                                                                    _ -> printError rule f

                (DoubleNegElim) , (Just (Row {formula : f })) -> case Partial.unsafePartial (fromJust (f)) of 
                                                                                    (Not (Not phi)) -> helperFunc listRows totalRows phi rule line
                                                                                    _ -> printError rule f
                (DoubleNegIntro) , (Just (Row {formula : f })) -> case Partial.unsafePartial (fromJust (f)) of 
                                                                                    phi -> helperFunc listRows totalRows (Not (Not phi)) rule line
                                                                                  --  _ -> printError rule f
                                                    
                _ , _ -> Left "catch em all"
              where 
                helperFunc :: Array Row -> Int -> Formula -> Rule -> Int -> Either String Proof 
                helperFunc listRow totalRows f r lineNr =  case insertAt (totalRows) (Row {rowNumber : totalRows+1 , formula : Just (f) , rule : Just r , applyRuleOn : [lineNr]}) listRow of 
                                                                                                               Just a -> Right (Proof {lines : a , assumptions : [] , proofNumber : pN})
                                                                                                               x1      -> Left "Error inserting a newline to existing proof"
                printError :: Rule -> Maybe Formula -> Either String Proof 
                printError r f = Left ("Cant apply rule:  " <> show rule <> "  on " <> show f)






evalBinaryRuleOnProof :: Rule -> Proof -> Int -> Int -> Either String Proof
evalBinaryRuleOnProof  rule (Proof {lines : listRows , assumptions : listAssumptions , proofNumber : pN }) line line2  = 
         let 
             totalRows = length listRows
         in case rule , index listRows (line-1) , index listRows (line2-1) of

                (AndIntro) , (Just (Row {formula : f })) , (Just (Row {formula : f2 })) -> case Partial.unsafePartial (fromJust (f)) , Partial.unsafePartial (fromJust (f2)) of 
                                                                                                                                        phi , psi  -> helperFunc listRows totalRows (And phi psi) rule line line2
                
                (ImplElim) , (Just (Row {formula : f })) , (Just (Row {formula : f2 })) -> case Partial.unsafePartial (fromJust (f)) , Partial.unsafePartial (fromJust (f2)) of 
                                                                                                                                        phi , (Implies phi1 psi)  -> if phi == phi1 
                                                                                                                                                                     then helperFunc listRows totalRows (psi) rule line line2
                                                                                                                                                                     else Left ("formula1 : " <> show phi <> " is not equal to the antecedent of formula2: " <> show (Implies phi1 psi))
                                                                                                                                        _ , _                     -> printError rule f f2
                (NegElim) , (Just (Row {formula : f })) , (Just (Row {formula : f2 })) -> case Partial.unsafePartial (fromJust (f)) , Partial.unsafePartial (fromJust (f2)) of 
                                                                                                                                        phi , (Not phi1)  -> if phi == phi1 
                                                                                                                                                                     then helperFunc listRows totalRows (Predicate "⊥" []) rule line line2
                                                                                                                                                                     else Left ("Wrong input formula.   Formula1 : " <> show phi <> " ,   Formula2 : " <> show (Not phi1))
                                                                                                                                        _ , _                     -> printError rule f f2
                (ModusTollens) , (Just (Row {formula : f })) , (Just (Row {formula : f2 })) -> case Partial.unsafePartial (fromJust (f)) , Partial.unsafePartial (fromJust (f2)) of 
                                                                                                                                        (Implies phi psi) , (Not psi1)  -> if psi == psi1 
                                                                                                                                                                     then helperFunc listRows totalRows (Not phi) rule line line2
                                                                                                                                                                     else Left ("Wrong input formula.   Formula1 : " <> show phi <> " ,   Formula2 : " <> show (Not phi))
                                                                                                                                        _ , _                     -> printError rule f f2
                                                    
                _ , _, _ -> Left "catch em all"
              where 
                helperFunc :: Array Row -> Int -> Formula -> Rule -> Int -> Int -> Either String Proof 
                helperFunc listRow totalRows f r  lineNr lineNr2 =  case insertAt (totalRows) (Row {rowNumber : totalRows+1 , formula : Just (f) , rule : Just r , applyRuleOn : [lineNr , lineNr2]}) listRow of 
                                                                                                               Just a -> Right (Proof {lines : a , assumptions : [] , proofNumber : pN})
                                                                                                               _      -> Left "Error inserting a newline to existing proof"
                printError :: Rule -> Maybe Formula -> Maybe Formula -> Either String Proof 
                printError r f f2 = Left ("Cant apply rule:  " <> show rule <> "  on the following formulas:  " <> show f <> "    " <> show f2)

-- Convenience/helper functions 

convertEitherProof :: Either String Proof -> Proof 
convertEitherProof (Right (Proof x)) = Proof x
convertEitherProof (Left _) = initProof   -- dummy value, return empty proof because some error occured. 


initProof :: Proof 
initProof = Proof {lines : [] , assumptions : [] , proofNumber : 1}

isEmptyProof :: Proof -> Boolean
isEmptyProof (Proof {lines : l , assumptions : a , proofNumber : n}) = case l , a , n of 
                                                                          [] , [] , _        -> true 
                                                                          _ , _ , _          -> false 

-- Prints the assumptions in a proof.

--showAssumptions :: Proof -> String
--showAssumptions (Proof {assumptions : listOfAssumptions})
--              | length listOfAssumptions == 0 = "" 
--              | length listOfAssumptions >= 1 = case listOfAssumptions of
--                                                  xs -> show xs
--showAssumptions _ = ""



-------------------------------------------- Proofs without any assumptions. Used for testing, remove later-------------------------------------------------------------

testProof1 :: Proof
testProof1 = Proof {lines : [Row {rowNumber : 1 , formula : Just (And (And (Predicate "P" []) (Predicate "Q" [])) (Predicate "R" [])) , rule : Just Premise , applyRuleOn : [] }, 
                             Row {rowNumber : 2 , formula : Just (And (Predicate "S" []) (Predicate "T" []) ) , rule : Just Premise , applyRuleOn : [2] } ] , assumptions : [] , proofNumber : 1}

-- Example 1.6 in book.

testProof2 :: Proof 
testProof2 = let 
                one = convertEitherProof (evalUnaryRuleOnProof (AndElimE1) testProof1 1)
                two = convertEitherProof (evalUnaryRuleOnProof (AndElimE2) one 3)
                three = convertEitherProof (evalUnaryRuleOnProof (AndElimE1) two 2)
                four = convertEitherProof (evalBinaryRuleOnProof (AndIntro) three 4 5)
             in four

-- Example 1.5 in book. 

testProof3 :: Proof 
testProof3 = let 
              one = Proof {lines : [Row {rowNumber : 1 , formula : Just (Predicate "P" []) , rule : Just Premise , applyRuleOn : [1] }, 
                             Row {rowNumber : 2 , formula : Just (Not(Not(And (Predicate "Q" []) (Predicate "R" [])))) , rule : Just Premise , applyRuleOn : [2] } ] 
                             , assumptions : [] , proofNumber : 1}
              two = convertEitherProof (evalUnaryRuleOnProof (DoubleNegIntro) one 1)
              three = convertEitherProof (evalUnaryRuleOnProof (DoubleNegElim) two 2)
              four = convertEitherProof (evalUnaryRuleOnProof (AndElimE2) three 4)
              five = convertEitherProof (evalBinaryRuleOnProof (AndIntro) four 3 5)
              in five





------------------------------------------- Proofs with assumptions. Used for testing, remove later----------------------------------------------------------------

-- Proofs with assumptions and their corresponding rules will be here later. 