module Proof where 

import Data.Either
import Data.Maybe
import Data.String
import Formula
import Prelude
import InferenceAlt(Rule(..))

import Ansi.Codes (EscapeCode(..))
import Control.Parallel (parOneOf)
import Data.Array (deleteAt, head, index, insert, insertAt, length, tail , range)
import Data.Maybe (fromJust)
import Partial.Unsafe as Partial


data Proof = Proof {lines :: Array Row , assumptions :: Array Box , proofNumber :: Int}
data Row = Row {rowNumber :: Int , formula :: Maybe Formula , rule :: Maybe Rule , applyRuleOn :: Array Int}
data Box = Box {lines :: Array Row , boxNumber :: Int}

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
                                                  xs -> "\n" <> "Assumption " <> show i <> ":\n" <> joinWith "\n" (map show xs) 






evalUnaryRuleOnProof :: Rule -> Proof -> Int -> Either String Proof
evalUnaryRuleOnProof rule1 (Proof rr@{lines : listRows , assumptions : listAssumptions , proofNumber : pN }) line  = 
         
         let 
             totalRows = length listRows
         in case rule1 , index listRows (line-1) of
                (AndElimE1) , (Just (testa@(Row {formula : f , rule : r1}))) -> case Partial.unsafePartial (fromJust (f)) of 
                                                                                    (And left _ ) -> let newLine2 = Row {rowNumber : totalRows+1 , formula : Just (left) , rule : Just rule1 , applyRuleOn : [line]}
                                                                                                     in case insertAt (totalRows) newLine2 listRows of 
                                                                                                               Just a -> Right (Proof {lines : a , assumptions : [] , proofNumber : pN})
                                                                                                               x1      -> Left "Error inserting a newline to existing proof"
                                                                                    x2 -> Left ("Cant apply rule:  " <> show rule1 <> "  on " <> show f)
                (AndElimE2) , (Just (testa@(Row {formula : f , rule : r1}))) -> case Partial.unsafePartial (fromJust (f)) of 
                                                                                    (And _ right ) -> let newLine2 = Row {rowNumber : totalRows+1 , formula : Just (right) , rule : Just rule1 , applyRuleOn : [line]}
                                                                                                     in case insertAt (totalRows) newLine2 listRows of 
                                                                                                               Just a -> Right (Proof {lines : a , assumptions : [] , proofNumber : pN})
                                                                                                               x1      -> Left "Error inserting a newline to existing proof"
                                                                                    x2 -> Left ("Cant apply rule:  " <> show rule1 <> "  on " <> show f)
                                                    
                x4 , x5 -> Left "Error, catch em all"


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



-------------------------------------------- Proofs without any assumptions. Used for testing-------------------------------------------------------------

testProof1 :: Proof
testProof1 = Proof {lines : [Row {rowNumber : 1 , formula : Just (And (And (Predicate "P" []) (Predicate "Q" [])) (Predicate "R" [])) , rule : Just Premise , applyRuleOn : [1] }, 
                             Row {rowNumber : 2 , formula : Just (And (Predicate "S" []) (Predicate "T" []) ) , rule : Just Premise , applyRuleOn : [2] } ] , assumptions : [] , proofNumber : 1}