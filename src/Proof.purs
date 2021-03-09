module Proof where 

import Data.Either
import Data.Maybe
import Data.String
import Formula
import Prelude


import Data.Array (deleteAt, head, index, insert, insertAt, length, tail, range)
import Data.Maybe (fromJust)
import InferenceAlt
import Partial.Unsafe as Partial


data Proof = Proof {lines :: Array Row , assumptions :: Array Box , proofNumber :: Int}
data Row = Row {rowNumber :: Int , formula :: Maybe Formula , rule :: Maybe Rule , applyRuleOn :: Array Int}
data Box = Box {lines :: Array Row , boxNumber :: Int , open :: Boolean}

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
testProof1 = Proof {lines : [Row {rowNumber : 1 , formula : Just (And (And (Predicate "P" []) (Predicate "Q" [])) (Predicate "R" [])) , rule : Just Premise , applyRuleOn : [1] }, 
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

-- Example 1.9 in book

testProof4 :: Proof
testProof4 = Proof {lines : [Row {rowNumber : 1 , formula : Just (Implies (Not (Predicate "Q" [])) (Not (Predicate "P" [])))  , rule : Just Premise , applyRuleOn : [1] }] , 
                             assumptions : [Box {lines : [Row {rowNumber : 2 , formula : Just (And (And (Predicate "P" []) (Predicate "Q" [])) (Predicate "R" [])) , rule : Just Assume , applyRuleOn : [2] }] , boxNumber : 1 , open : true }] 
                             
                             
                             , proofNumber : 1}