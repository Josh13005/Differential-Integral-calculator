{- Assignment 5
 - Name: Joshua Sam Varughese 
 - Date: 1-11-2019 
 -}
module Assign_5 where


{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - Description: The function definiteIntegral basically finds the area enclosed by the given function in  a given interval.
                Here [a,b] is the interval where the user enter the values for a and b. 
                n is the nuber of rectangkes between  the given interval. 
                Here, g is given by the user by means of lamba expressions. 
                In this function, list comprehension is used to develop a list 
                fromInteger is used to covert 'n' of "Integer" type to 'a'. The type is -- fromInteger :: Num a => Integer -> a 

 -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n 
 | a == b = 0 
 | a < b = sum[(((g (a+(fromInteger i) * deltaX) + g(a+((fromInteger i)-1)*deltaX))/2)*deltaX) | i <- [1..n]]
  where 
    deltaX = (b-a)/ (fromInteger n) 
        

{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - Description: The function funH is used to get the area between the curves " y = nth root of x " and "y = x to the power n" from 0 to 1 
                Here a = 0 and b = 1 
                If n <= 0, it is undefined which is the base case in this function 
                The definiteIntegral function is used here to obtain the area enclosed between each curve
                The two curves are defined using lambda expression. 
 -}
funH :: Integer -> Double
funH n 
 | n > 0 = definiteIntegral 0 1 (\x -> x**( 1 / (fromInteger n))) 100 -  definiteIntegral 0 1 (\x -> x**(fromInteger n)) 100
 | otherwise = error "Function undefined"
    

 {- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - Description: The function funK is used to get the area between the curves " y = n to the power x " and x axis from -1 to 1 
                Here a = -1 and b = 1 
                If n <= 0, it is undefined which is the base case in this function 
                The definiteIntegral function is used here to obtain the area enclosed between the curve and the x axis. 
                The curve is expressed using lambda expression. 
 -}
funK :: Double -> Double
funK n 
 | n > 0 = definiteIntegral (-1) 1 (\x -> n**x ) 100
 | otherwise = error "Function undefined"   


 {- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: definiteIntegral 
 - - Test Case Number: 1
 - - Input: definiteIntegral 0 6 (\x->x*x) 10
 - - Expected Output: 72.36
 - - Acutal Output: 72.36
 - -----------------------------------------------------------------
 - - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 2
 - - Input: definiteIntegral 2 2 (\x->x+2) 10
 - - Expected Output: 0.0
 - - Acutal Output: 0.0 
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 3
 - - Input: definiteIntegral 3 7 (\x->x*x + 2) 100
 - - Expected Output: 113.33439999999995
 - - Acutal Output: 113.33439999999995
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 1
 - - Input: funH 2
 - - Expected Output: 0.3331129471031475
 - - Acutal Output: 0.3331129471031475
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 2
 - - Input: funH 0
 - - Expected Output: Exception: Function undefined
 - - Acutal Output: Exception: Function undefined
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 3
 - - Input: funH (-12)
 - - Expected Output: Exception: Function undefined
 - - Acutal Output: Exception: Function undefined
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 1
 - - Input: funK 12
 - - Expected Output: 4.7966064717284125
 - - Acutal Output: 4.7966064717284125
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 2
 - - Input: funK 0
 - - Expected Output: Exception: Function undefined
 - - Acutal Output: Exception: Function undefined
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 3
 - - Input: funK (-8)
 - - Expected Output: Exception: Function undefined
 - - Acutal Output: Exception: Function undefined
 - -----------------------------------------------------------------
 -}

