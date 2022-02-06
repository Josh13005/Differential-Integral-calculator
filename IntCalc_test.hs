{- Assignment 5 Tests
 - Name: Joshua Sam Varughese 
 - Date: 1-11-2019 
 -}

import Assign_5

import Test.QuickCheck



prop1 :: Integer -> Bool
prop1 n = definiteIntegral 1 1 (\x -> x) n == 0 

prop2 :: Integer -> Property 
prop2 n = n>0 ==> funH n < 1

prop3 :: Double -> Property 
prop3 n = n>0 ==> funK n > 1

--prop4 :: Double -> Double -> Property
--prop4 a b = a /= b ==> definiteIntegral a b (\x -> x) 1000 > 0

main :: IO ()
main = 
    do
        quickCheck prop1
        quickCheck prop2
        quickCheck prop3 


{-
Test Cases:

---------------------------------------------------------------------------------------------------------------
Function: definiteIntegral
Property: definiteIntegral 1 1 (\x -> x) n == 0 
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------   
---------------------------------------------------------------------------------------------------------------
Function: funH 
Property:  n>0 ==> funH n < 1
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------   
---------------------------------------------------------------------------------------------------------------
Function: funK 
Property: n>0 ==> funK n > 1
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------        
-}        
