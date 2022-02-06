{- Assignment 4 Tests
 - Name: Joshua Sam Varughese
 - Date: 17-11-2019
 -}

import Assign_4
import Assign_4_ExtraCredit

import Test.QuickCheck
  -- see https://www.stackage.org/haddock/lts-8.24/criterion-1.1.4.0/Criterion-Main.html


valueProp :: Double -> Bool
valueProp n = value X n == n
valueProp1 :: Double -> Bool
valueProp1 n = value (Coef 0.0) n == 0
valueProp2 :: Double -> Double -> Bool
valueProp2 a b = value (Sum (Coef a) (Coef b)) 3.0 == a + b
valueProp3 :: Double -> Double -> Bool
valueProp3 a b = value (Prod (Coef a) (Coef b)) 1.0 == a*b 
valueProp4 :: Double -> Property
valueProp4 a = a /= 0 ==> value (Quot (Coef a) (Coef a)) 9.0 == 1.0 
valueProp5 :: Double -> Property
valueProp5 n = n > 0 ==> value (Log (Coef n)) n == (log (n))  
valueProp6 :: Double -> Bool
valueProp6 n = value (Exp (Coef n)) n == (exp (n))
            
simpProp :: Double -> Bool
simpProp n = simp (Coef n) == Coef n
simpProp1 :: Double -> Bool
simpProp1 a = simp (Sum (Coef a) (Coef 0.0)) == Coef a
simpProp2 :: Double -> Bool 
simpProp2 a = simp (Prod (Coef a) (Log(Coef 1.0))) == Coef 0.0 
simpProp3 :: Double -> Bool 
simpProp3 a = simp (Prod (Coef a) (Exp (Coef 0.0))) == Coef a
simpProp4 :: Double -> Bool
simpProp4 a = simp (Quot (Coef a) (Coef 1.0)) == Coef a
simpProp5 :: Double -> Property 
simpProp5 a = a /= 0 ==> simp (Quot (Coef 0.0) (Coef a)) == Coef 0.0

diffProp :: Double -> Bool 
diffProp n = diff(Coef n) == Coef 0.0
diffProp1 :: Double -> Bool
diffProp1 n = diff X == Coef 1.0 
diffProp2 :: Double -> Double -> Bool 
diffProp2 n a = simp (diff (Prod (Sum (diff (Coef 0.0)) (Coef 0.0)) (diff X))) == Coef 0.0
diffProp3 :: Double -> Double -> Bool 
diffProp3 n a = simp (diff (Quot (Sum (diff (Coef n)) (Coef a)) (diff X))) == Coef 0.0 
diffProp4 :: Double -> Double -> Bool
diffProp4 a b = simp(diff(Sum (diff (Exp (Coef a)))(Coef b))) == Coef 0.0  
diffProp5 :: Double -> Bool
diffProp5 a = simp(diff(Log (Coef a))) == Coef 0.0

main :: IO ()
main = 
    do 
        quickCheck valueProp
        quickCheck valueProp1
        quickCheck valueProp2
        quickCheck valueProp3
        quickCheck valueProp4
        quickCheck valueProp5
        quickCheck valueProp6
        quickCheck simpProp
        quickCheck simpProp1
        quickCheck simpProp2
        quickCheck simpProp3
        quickCheck simpProp4
        quickCheck simpProp5
        quickCheck diffProp
        quickCheck diffProp1
        quickCheck diffProp2
        quickCheck diffProp3
        quickCheck diffProp4
        quickCheck diffProp5




{-
Test Cases:

---------------------------------------------------------------------------------------------------------------
Function: value
Property: valueProp n = value X n == n
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: value
Property:
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: value
Property: valueProp1 n = value (Coef 0.0) n == 0
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: value
Property: valueProp2 a b = value (Sum (Coef a) (Coef b)) 3.0 == a + b
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: value
Property: valueProp3 a b = value (Prod (Coef a) (Coef b)) 1.0 == a*b
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: value 
Property: valueProp4 a = a /= 0 ==> value (Quot (Coef a) (Coef a)) 9.0 == 1.0 
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: value
Property: valueProp5 n = n > 0 ==> value (Log (Coef n)) n == (log (n))  
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: value
Property: valueProp6 n = value (Exp (Coef n)) n == (exp (n))
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: simp
Property: simpProp n = simp (Coef n) == Coef n
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: simp
Property: simpProp1 a = simp (Sum (Coef a) (Coef 0.0)) == Coef a
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: simp
Property: simpProp2 a = simp (Prod (Coef a) (Log(Coef 1.0))) == Coef 0.0 
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: simp
Property: simpProp3 a = simp (Prod (Coef a) (Exp (Coef 0.0))) == Coef a
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: simp
Property: simpProp4 a = simp (Quot (Coef a) (Coef 1.0)) == Coef a
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: simp
Property: simpProp5 a = a /= 0 ==> simp (Quot (Coef 0.0) (Coef a)) == Coef 0.0
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: diff
Property: diffProp n = diff(Coef n) == Coef 0.0
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: diff
Property: diffProp1 n = diff X == Coef 1.0 
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function:
Property: diffProp2 n a = simp (diff (Prod (Sum (diff (Coef 0.0)) (Coef 0.0)) (diff X))) == Coef 0.0
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: diff
Property: diffProp3 n a = simp (diff (Quot (Sum (diff (Coef n)) (Coef a)) (diff X))) == Coef 0.0 
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: diff
Property: diffProp4 a b = simp(diff(Sum (diff (Exp (Coef a)))(Coef b))) == Coef 0.0  
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
Function: diff
Property: diffProp5 a = simp(diff(Log (Coef a))) == Coef 0.0
Actual test result : Pass
----------------------------------------------------------------------------------------------------------------
-}





         
         
         
         
         
         
         
         
         
 


