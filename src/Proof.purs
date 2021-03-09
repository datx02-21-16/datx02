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
                                xs , ys -> "\n" <> joinWith "\n" (map show xs) <> "\n" <> "\n" <> joinWith "\n" (map show ys)     -- added "\n" to the left of joinWith so we can print out proofs niceer when returning them from the evalRule functions. 

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
                                                                                                                                        
                
                                                    
                _ , _, _ -> Left "catch em all"
              where 
                helperFunc :: Array Row -> Int -> Formula -> Rule -> Int -> Int -> Either String Proof 
                helperFunc listRow totalRows f r  lineNr lineNr2 =  case insertAt (totalRows) (Row {rowNumber : totalRows+1 , formula : Just (f) , rule : Just r , applyRuleOn : [lineNr , lineNr2]}) listRow of 
                                                                                                               Just a -> Right (Proof {lines : a , assumptions : [] , proofNumber : pN})
                                                                                                               _      -> Left "Error inserting a newline to existing proof"

-- Convenience/helper functions 

-- The evalRules spit out type Either String Proof, and applying this on an evalRule again wont work because its type signature taking a proof. This function
-- converts it to correct type as Proof.

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



-------------------------------------------- Proofs without any assumptions. Used for testing-------------------------------------------------------------

testProof1 :: Proof
testProof1 = Proof {lines : [Row {rowNumber : 1 , formula : Just (And (And (Predicate "P" []) (Predicate "Q" [])) (Predicate "R" [])) , rule : Just Premise , applyRuleOn : [1] }, 
                             Row {rowNumber : 2 , formula : Just (And (Predicate "S" []) (Predicate "T" []) ) , rule : Just Premise , applyRuleOn : [2] } ] , assumptions : [] , proofNumber : 1}

-- Example 1.6 in book, using and elim/intro

testProof2 :: Proof 
testProof2 = let 
                one = convertEitherProof (evalUnaryRuleOnProof (AndElimE1) testProof1 1)
                two = convertEitherProof (evalUnaryRuleOnProof (AndElimE2) one 3)
                three = convertEitherProof (evalUnaryRuleOnProof (AndElimE1) two 2)
                four = convertEitherProof (evalBinaryRuleOnProof (AndIntro) three 4 5)
             in four

------------------------------------------- Proofs with assumptions. Used for testing----------------------------------------------------------------

testProof3 :: Proof
testProof3 = Proof {lines : [Row {rowNumber : 1 , formula : Just (And (And (Predicate "P" []) (Predicate "Q" [])) (Predicate "R" [])) , rule : Just Premise , applyRuleOn : [1] }] , 
                             assumptions : [Box {lines : [Row {rowNumber : 2 , formula : Just (And (And (Predicate "P" []) (Predicate "Q" [])) (Predicate "R" [])) , rule : Just Assume , applyRuleOn : [2] }] , boxNumber : 1 , open : true }] 
                             
                             
                             , proofNumber : 1}


testProof5 :: Proof 
testProof5 = Proof {lines : [Row {rowNumber : 1 , formula : Just (And (Predicate "P" []) (Predicate "Q" [])) , rule : Just AndElimE1 , applyRuleOn : [0] },
                             Row {rowNumber : 2 , formula : Just (Or (Predicate "P" []) (Predicate "Q" []))  , rule : Just AndElimE1 , applyRuleOn : [0] },
                             Row {rowNumber : 3 , formula : Just (And (Predicate "P" []) (Predicate "Q" [])) , rule : Just AndElimE1 , applyRuleOn : [0] },
                             Row {rowNumber : 4 , formula : Just (Or (Predicate "P" []) (Predicate "Q" []))  , rule : Just AndElimE1 , applyRuleOn : [0] }
] , assumptions : [Box {lines : [Row {rowNumber : 5 , formula : Just (And (Predicate "P" []) (Predicate "Q" [])) , rule : Just Assume , applyRuleOn : [5]}, 
                                 Row {rowNumber : 6 , formula : Just (And (Predicate "P" []) (Predicate "Q" [])) , rule : Just AndElimE1 , applyRuleOn : [5]},
                                 Row {rowNumber : 7 , formula : Just (And (Predicate "P" []) (Predicate "Q" [])) , rule : Just AndElimE1 , applyRuleOn : [5]},
                                 Row {rowNumber : 8 , formula : Just (And (Predicate "P" []) (Predicate "Q" [])) , rule : Just AndElimE1 , applyRuleOn : [5]}] , boxNumber : 1 , open : false},



                   Box {lines : [Row {rowNumber : 9 , formula : Just (Or (Predicate "P" []) (Predicate "Q" [])) , rule : Just Assume , applyRuleOn : [6]}] , boxNumber : 2 , open : true}] , proofNumber : 1}