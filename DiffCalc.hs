{- Assignment 4
 - Name: Joshua Sam Varughese
 - Date: 17-11-2019

 -}
module Assign_4 where


data MathExpr a = X
                | Coef a
                | Sum (MathExpr a) (MathExpr a)
                | Prod (MathExpr a) (MathExpr a)
                | Quot (MathExpr a) (MathExpr a)
                | Exp (MathExpr a)
                | Log (MathExpr a)
                deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - value
 - -----------------------------------------------------------------
 - Description: The value function gives the exact value of the given input at a particular point. 
                It takes the input of type MathExpr which is an algebraic data type. 
                The function value has various Data Constructors like Coef, Prod, Sum, Quot, Exp, Log.
 -}
value :: (Floating a, Eq a) => MathExpr a -> a -> a
value X n = n
value (Coef a) n = a
value (Sum x y) n = value x n + value y n  
value (Prod x y) n = value x n * value y n 
value (Quot x y) n = value x n / value y n 
value (Exp x) n  = exp $ value x n
value (Log x) n = log $ value x n
    
    
    
    

{- -----------------------------------------------------------------
 - simp
 - -----------------------------------------------------------------
 - Description: The simp function is used to simply the input values. 
                It takes the input of type MathExpr which is an algebraic data type. 
                The output is of MathExpr a type.
                
 -}
simp :: (Floating a, Eq a) => MathExpr a -> MathExpr a
simp (Coef u) = Coef u
simp (Sum (Coef  0.0) u) = simp u
simp (Sum u (Coef  0.0)) = simp u
simp (Sum u v) =
        let
            u' = simp u
            v' = simp v
        in
            if u' == u && v' == v then (Sum u v) 
            else  simp (Sum u' v')
simp (Prod (Coef 0.0) _ ) = Coef 0.0
simp (Prod _ (Coef 0.0)) = Coef 0.0 
simp (Prod u (Coef 1.0)) = simp u 
simp (Prod u v) = 
    let 
        u' = simp u 
        v' = simp v
    in 
        if u' == u && v' == v then (Prod u v) 
        else  simp (Prod u' v')
simp (Quot (Coef 0.0) _ ) = Coef 0.0 
simp (Quot u (Coef 1.0)) =  simp u
simp (Quot u v) = 
        let 
            u' = simp u 
            v' = simp v
        in 
            if u' == u && v' == v then (Quot u v) 
            else  simp (Quot u' v')

simp (Exp (Coef 0.0)) = Coef 1.0
simp (Exp u) = 
    let 
        u' = simp u 
    in 
        if u' == u then (Exp u) 
        else  simp (Exp u') 

simp (Log (Coef 1.0)) = Coef 0.0
simp (Log u) = 
    let 
        u' = simp u 
    in 
        if u' == u then (Log u) 
        else  simp (Log u') 

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description: The diff function basically differntiates the given values using the various differentiation rules. 
                It takes the input of type MathExpr which is an algebraic data type. 
                It aslo returns the value in MathExpr a type.
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff (Coef u) = Coef 0.0 
diff X = Coef 1.0
diff (Sum u v) = Sum (diff u) (diff v)
diff (Prod u v) = Sum (Prod (diff u) v) $ Prod u (diff v)
diff (Quot u v) = Quot (Sum(Prod (diff u) v) (Prod (Coef(-1))((Prod u (diff v))))) $ Prod v v
diff (Exp u) = Prod (Exp u) (diff u)
diff (Log u) = Quot (diff u) u 

{- -----------------------------------------------------------------
 - readDiffWrite
 - -----------------------------------------------------------------
 - Description: The readDiffWrite function reads the input from a file f and applies the diff function followed by simp function.
                It writes the final value to file g followed by a new line character "\n" which prints a new line.
 -}
readDiffWrite :: FilePath -> FilePath -> IO ()
readDiffWrite f g = 
    if f == g then error " Same File" else
    do 
        fileContents <- readFile f
        let u = read fileContents :: MathExpr Double
        let v = show $ simp $ diff u 
        writeFile g (v ++ "\n") 
    
        

{- 
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 1
 - - Input: value (Sum (Coef 2.0) (Coef 3.0)) 2.0 
 - - Expected Output: 5.0
 - - Acutal Output: 5.0 
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 2
 - - Input: value (Prod (Coef 3.0) (Coef 3.0)) 1.0 
 - - Expected Output: 9.0
 - - Acutal Output: 9.0
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function:value
 - - Test Case Number: 3
 - - Input: value (Quot (Coef 9.0) (Coef 0.0)) 1.0 
 - - Expected Output: Infinity 
 - - Acutal Output: Infinity 
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function:value
 - - Test Case Number: 4
 - - Input:  value (Log (Coef 3.0)) 2.0
 - - Expected Output: 1.0986122886681098 
 - - Acutal Output: 1.0986122886681098
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function:value
 - - Test Case Number: 5
 - - Input:  value (Exp (Coef 3.0)) 2.0
 - - Expected Output: 20.085536923187668
 - - Acutal Output: 20.085536923187668
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function:value
 - - Test Case Number: 6
 - - Input:  value 
 - - Expected Output: 1.0986122886681098 
 - - Acutal Output: 1.0986122886681098
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 7
 - - Input: value X 4.0
 - - Expected Output: 4.0
 - - Acutal Output: 4.0
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 1
 - - Input: simp (Prod (Coef 0.0)(Coef 5.0))
 - - Expected Output: Coef 0.0
 - - Acutal Output: Coef 0.0 
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 2
 - - Input: simp (Sum (Prod(Coef 0.0)(Coef 3.0))(Coef 5.0))
 - - Expected Output: Coef 5.0
 - - Acutal Output: Coef 5.0
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 3
 - - Input: simp (Quot (Coef 2.0) (Coef 5.0))
 - - Expected Output: Quot (Coef 2.0) (Coef 5.0)
 - - Acutal Output: Quot (Coef 2.0) (Coef 5.0)
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 4
 - - Input: simp (Exp (Prod (Coef 2.0)(Coef 5.0)))
 - - Expected Output: Exp (Prod (Coef 2.0) (Coef 5.0))
 - - Acutal Output: Exp (Prod (Coef 2.0) (Coef 5.0))
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 5
 - - Input: simp (Quot (Coef 3.0) (Coef 1.0))
 - - Expected Output: Coef 3.0
 - - Acutal Output: Coef 3.0 
 - -----------------------------------------------------------------
--------------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 6
 - - Input: simp (Quot (Coef 0.0) (Coef 3.0))
 - - Expected Output: Coef 0.0
 - - Acutal Output: Coef 0.0
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 7
 - - Input: simp (Exp (Coef 0.0))
 - - Expected Output: Coef 1.0
 - - Acutal Output: Coef 1.0 
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 8
 - - Input: simp (Log (Coef 1.0))
 - - Expected Output: Coef 0.0
 - - Acutal Output: Coef 0.0
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 1
 - - Input:  diff (Coef 3.0)
 - - Expected Output: Coef 0.0
 - - Acutal Output: Coef 0.0
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 2
 - - Input: diff (Sum (Sum (Coef 3.0)(Coef 2.0)) (Prod (Coef 2.0)(Coef 5.0)))
 - - Expected Output: Sum (Sum (Coef 0.0) (Coef 0.0)) (Sum (Prod (Coef 0.0) (Coef 5.0)) (Prod (Coef 2.0) (Coef 0.0))) 
 - - Acutal Output: Sum (Sum (Coef 0.0) (Coef 0.0)) (Sum (Prod (Coef 0.0) (Coef 5.0)) (Prod (Coef 2.0) (Coef 0.0)))
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 3
 - - Input: diff X
 - - Expected Output: Coef 1.0
 - - Acutal Output: Coef 1.0
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 4
 - - Input: diff (Prod (Sum (Coef 3.0)(Coef 2.0)) (Sum (Coef 2.0)(Coef 5.0)))
 - - Expected Output: Sum (Prod (Sum (Coef 0.0) (Coef 0.0)) (Sum (Coef 2.0) (Coef 5.0))) (Prod (Sum (Coef 3.0) (Coef 2.0)) (Sum (Coef 0.0) (Coef 0.0)))
 - - Acutal Output: Sum (Prod (Sum (Coef 0.0) (Coef 0.0)) (Sum (Coef 2.0) (Coef 5.0))) (Prod (Sum (Coef 3.0) (Coef 2.0)) (Sum (Coef 0.0) (Coef 0.0)))
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 5
 - - Input: diff (Quot (Sum (Coef 3.0)(Coef 2.0)) (Sum (Prod (Coef 3.0)(Coef 4.0))(Coef 5.0)))
 - - Expected Output: Quot (Sum (Prod (Sum (Coef 0.0) (Coef 0.0)) (Sum (Prod (Coef 3.0) (Coef 4.0)) (Coef 5.0))) (Prod (Coef (-1.0)) (Prod (Sum (Coef 3.0) (Coef 2.0)) (Sum (Sum (Prod (Coef 0.0) (Coef 4.0)) (Prod (Coef 3.0) (Coef 0.0))) (Coef 0.0))))) (Prod (Sum (Prod (Coef 3.0) (Coef 4.0)) (Coef 5.0)) (Sum (Prod (Coef 3.0) (Coef 4.0)) (Coef 5.0)))
 - - Acutal Output: Quot (Sum (Prod (Sum (Coef 0.0) (Coef 0.0)) (Sum (Prod (Coef 3.0) (Coef 4.0)) (Coef 5.0))) (Prod (Coef (-1.0)) (Prod (Sum (Coef 3.0) (Coef 2.0)) (Sum (Sum (Prod (Coef 0.0) (Coef 4.0)) (Prod (Coef 3.0) (Coef 0.0))) (Coef 0.0))))) (Prod (Sum (Prod (Coef 3.0) (Coef 4.0)) (Coef 5.0)) (Sum (Prod (Coef 3.0) (Coef 4.0)) (Coef 5.0)))
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 6
 - - Input: diff (Exp (Sum (Coef 3.0)(Coef 2.0 )))
 - - Expected Output: Prod (Exp (Sum (Coef 3.0) (Coef 2.0))) (Sum (Coef 0.0) (Coef 0.0))
 - - Acutal Output: Prod (Exp (Sum (Coef 3.0) (Coef 2.0))) (Sum (Coef 0.0) (Coef 0.0))
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 7
 - - Input: diff (Log (Prod (Coef 3.0)(Coef 2.0 )))
 - - Expected Output: Quot (Sum (Prod (Coef 0.0) (Coef 2.0)) (Prod (Coef 3.0) (Coef 0.0))) (Prod (Coef 3.0) (Coef 2.0))
 - - Acutal Output: Quot (Sum (Prod (Coef 0.0) (Coef 2.0)) (Prod (Coef 3.0) (Coef 0.0))) (Prod (Coef 3.0) (Coef 2.0))
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: readDiffWrite
 - - Test Case Number: 1
 - - Input File Names: "1Jc3.txt"  "output.txt"
 - - Input File Contents: Log (Prod (Coef 3.0)(Coef 2.0 ))
 - - Expected File Output: Coef 0.0
 - - Acutal File Output: Coef 0.0
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: readDiffWrite
 - - Test Case Number: 2
 - - Input File Names: "happy.txt" "sad.txt"
 - - Input File Contents: Prod (Exp (Sum (Coef 3.0) (Coef 2.0))) (Sum (Coef 0.0) (Coef 0.0)))
 - - Expected File Output: Coef 0.0
 - - Acutal File Output: Coef 0.0
 - -----------------------------------------------------------------
 -----------------------------------------------------------------
 - - Function: readDiffWrite
 - - Test Case Number: 3
 - - Input File Names: "me.txt" "you.txt"
 - - Input File Contents : Sum (Prod (Coef 3.0) X) (Prod (Coef 3.0) (Coef 1.0))
 - - Expected Output: Coef 3.0
 - - Acutal Output: Coef 3.0
 - -----------------------------------------------------------------
 -}
 
 