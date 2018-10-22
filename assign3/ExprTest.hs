{--|
Module : ExprTest
Description: Contains tests for this library
Copyright: (c) Noa Barsky @2018
License : WTFPL 
Maintainer : noabarsky123[at]gmail[dot]com
Stability : experimental
Portability : POSIX
-}

module ExprTest (evalProp1, evalProp2, evalProp3, evalProp4, evalProp5, evalProp6, 
                 evalProp7, evalProp8, evalProp9, diffProp1, diffProp2, diffProp3,
                 diffProp4, diffProp5, diffProp6, diffProp7, diffProp8,diffProp9,
                 diffProp10,diffProp11, simpProp1,simpProp2,simpProp3,simpProp4,
                 simpProp5,simpProp6, simpProp8, simpProp9,simpProp10,
                 simpProp11, simpProp12, simpProp13, simpProp14, simpProp15, simpProp16,
                 simpProp17, simpProp18, simpProp19, simpProp20, simpProp21, simpProp22,
                 simpProp23, simpProp25, simpProp26, multDiffProp1, antiDerivProp1, antiDerivProp2, antiDerivProp3,
                 antiDerivProp4, antiDerivProp6, antiDerivProp7, antiDerivProp8, antiDerivProp9,
                 antiDerivProp10, antiDerivProp11, antiDerivProp12, parseProp1, parseProp2, parseProp3,
                 parseProp4, parseProp5, parseProp6,  parseProp8, parseProp9,
                 parseProp10, parseProp11, parseProp12, parseProp13) where

import           ExprDiff
import           ExprParser
import           ExprType
import           ExprTrig
import           ExprPretty

import qualified Data.Map.Strict as Map
import           Test.QuickCheck
{-The following are test cases for many features of this library. Not everything is tested and
not all cases are covered, unfortnately I ran out of time. Feel free to get test further 
and give me your feed back on the bugs. The idea is that since all the expression will boil
down to vars and const that if those cases work, then nested expressions will work as well due
to the recursive nature of the functions.-}

{-eval function in ExprDiff, test cases-}

{-| Tests if eval works on add with variables where the value are defined in the dictionary-}
evalProp1 :: Double -> Double -> Bool
evalProp1 a b = eval (Map.fromList [("x",a),("y",b)]) (Add (Var "x") (Var "y")) == a+b
testevalProp1 = quickCheck evalProp1

{-| Tests if eval works on mult with variables where the value are defined in the dictionary-}
evalProp2 :: Double -> Double -> Bool
evalProp2 a b = eval (Map.fromList [("x",a),("y",b)]) (Mult (Var "x") (Var "y")) == a*b
testevalProp2 = quickCheck evalProp2

{-| Tests if eval works on exp with a variable where the value are defined in the dictionary-}
evalProp3 :: Double -> Bool
evalProp3 a= eval (Map.fromList [("x",a)]) (Exp (Var "x")) == exp(a)
testevalProp3 = quickCheck evalProp3

{-| Tests if eval works on exp with a variable where the value are defined in the dictionary, with an additional non-necessary 
    variable in the dictionary-}
evalProp4 :: Double -> Double -> Bool
evalProp4 a b = eval (Map.fromList [("x",a),("y",b)]) (Exp (Var "x")) == exp(a)
testevalProp4 = quickCheck evalProp4

{-| Tests to see if var will just return what it is in the dicionary-}
evalProp5 :: Double -> Double -> Bool
evalProp5 a b = eval (Map.fromList [("x",a),("y",b)]) (Var "y") == b
testevalProp5 = quickCheck evalProp5

{-| Tests if the Neg evaluated just returns the negative of the constant-}
evalProp6 :: Double -> Double -> Bool
evalProp6 a b = eval (Map.fromList [("x",a),("y",b)]) (Neg (Const a)) == -a
testevalProp6 = quickCheck evalProp6

{-| Tests is Cos evaluated just returns the cosine of that value-}
evalProp7 :: Double -> Double -> Bool
evalProp7 a b = eval (Map.fromList [("x",a),("y",b)]) (Cos (Var "x")) == cos (a)
testevalProp7 = quickCheck evalProp7

{-| Tests is Cos evaluated just returns the sine of that value-}
evalProp8 :: Double -> Double -> Bool
evalProp8 a b = eval (Map.fromList [("x",a),("y",b)]) (Sin (Var "x")) == sin (a)
testevalProp8 = quickCheck evalProp8

{-| Tests the exponent of that value, not a quickcheck test, must input parameters to test-}
evalProp9 :: Double -> Double -> Bool
evalProp9 a b = eval (Map.fromList [("x",a),("y",b)]) (Exponent (Var "x") (Var "y")) == a**b

{-partDiff function in ExprDiff test cases-}

{-| Tests to see if the derivative of any constant is 0-}
diffProp1 :: String -> Double -> Bool
diffProp1 s a = partDiff s (Const a) == Const 0
testdiffProp1 = quickCheck diffProp1 

{-| Tests to see if that if the parameter s isn't "t" that it will be 0.0 and as such
    the function will return True if s=="t" it will return false-}
diffProp2 ::  String -> Bool
diffProp2 s = partDiff "t" (Var s) == Const 0.0

{-| Tests to see that if Var is s is the same as the s parameter in partDiff
    it will return Const 1-}
diffProp3 :: String -> Bool
diffProp3 s = partDiff s (Var s) == Const 1
testdiffProp3 = quickCheck diffProp3 

{-| Tests that the derivative of e^x will just return e^x, where x is a variable-}
diffProp4 :: String -> Double -> Bool
diffProp4 s a = partDiff s (Exp (Var s)) == Mult (Exp (Var s)) (Const 1)
testdiffProp4 = quickCheck diffProp4 

{-| Tests that the derivative of e^c will just return 0, where c is a constant-}
diffProp5 :: String -> Double -> Bool
diffProp5 s a = partDiff s (Exp (Const a)) == Mult (Exp (Const a)) (Const 0)
testdiffProp5 = quickCheck diffProp5

{-| Tests that the derivative of y=c*x will just return c, where c is a constant and x is a variable-}
diffProp6 :: String -> Double -> Bool
diffProp6 s a = partDiff s (Mult (Const a) (Var s)) == Add (Mult (Const 0) (Var s)) (Mult (Const a) (Const 1))
testdiffProp6 = quickCheck diffProp6

{-| Tests that the derivative of y=c + x will just return 1, where c is a constant and x is a variable-}
diffProp7 :: String -> Double -> Bool
diffProp7 s a = partDiff s (Add (Const a) (Var s)) == Add (Const 0) (Const 1)
testdiffProp7 = quickCheck diffProp7

{-| Tests that the derivative of cos(x) will just return -sin(x), x is a variable-}
diffProp8 :: String -> Double -> Bool
diffProp8 s a = partDiff s (Cos (Var s)) == Mult (Neg (Sin (Var s))) (Const 1)
testdiffProp8 = quickCheck diffProp8

{-| Tests that the derivative of sin(x) will just return cos(x), x is a variable-}
diffProp9 :: String -> Double -> Bool
diffProp9 s a = partDiff s (Sin (Var s)) == Mult (Cos (Var s)) (Const 1)
testdiffProp9 = quickCheck diffProp9

{-| Tests that the derivative of lnx will just return 1/x, x is a variable-}
diffProp10 :: String -> Double -> Bool
diffProp10 s a = partDiff s (Ln (Var s)) == Mult (Exponent (Var s) (Const (-1))) (Const 1)
testdiffProp10 = quickCheck diffProp10

{-| Tests that the derivative of 2^x will just return 2^xln2, x is a variable-}
diffProp11 :: String -> Double -> Bool
diffProp11 s a = partDiff s (Exponent (Const a) (Var s)) == Mult (Mult ((Exponent (Const a) (Var s))) (Ln (Const a))) (partDiff s (Var s))
testdiffProp11 = quickCheck diffProp11

{-| Tests that anything mulitple by 0 will return 0-}
simpProp1 :: Double -> Double -> String -> Bool
simpProp1 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Mult (Var s) (Const 0)) == Const 0
testsimpProp1 = quickCheck simpProp1 

{-| Tests the associativity to make sure the that no matter the order, anything mulitplied by 0 will return 0-}
simpProp2 :: Double -> Double -> String -> Bool
simpProp2 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Mult (Const 0) (Var s)) == Const 0
testsimpProp2 = quickCheck simpProp2

{-| Tests that anything multiplied by 1 will return itself-}
simpProp3 :: Double -> Double -> String -> Bool
simpProp3 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Mult (Var "x") (Const 1)) == Const a
testsimpProp3 = quickCheck simpProp3

{-| Tests the associativity to make sure the that no matter the order, anything mulitplied by 1 will return will return itself-}
simpProp4 :: Double -> Double -> String -> Bool
simpProp4 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Mult (Const 1) (Var "x")) == Const a
testsimpProp4 = quickCheck simpProp4

{-| Tests to see that if the variable is not in the dictionary it will just return the variable as is-}
simpProp5 :: Double -> Double -> String -> Bool
simpProp5 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Var s) == Var s
testsimpProp5 = quickCheck simpProp5

{-| Tests to see that is variable is in the dicionary then it just returns the value as a constant-}
simpProp6 :: Double -> Double -> String -> Bool
simpProp6 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Var "x") == (Const a)
testsimpProp6 = quickCheck simpProp6

{-| Tests that anything added with 0 will just return itself-}
simpProp8 :: Double -> Double -> String -> Bool
simpProp8 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Add (Const 0) (Var "y")) == Const b
testsimpProp8 = quickCheck simpProp8

{-| Tests the associativity to make sure the that no matter the order, anything added to 0 will return will return itself-}
simpProp9 :: Double -> Double -> String -> Bool
simpProp9 a b s = simplify (Map.fromList [(s,a),("y",b)]) (Add (Var s) (Const 0)) == Const a
testsimpProp9 = quickCheck simpProp9

{-| Tests that ln(1) will always 0-}
simpProp10 :: Double -> Double -> String -> Bool
simpProp10 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Ln (Const 1)) == Const 0

{-| Test that e^1 returns 0-}
simpProp11 :: Double -> Double -> String -> Bool
simpProp11 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Exp (Const 0)) == Const 1

{-| Tests that anything to the exponent of 0 returns 1-}
simpProp12 :: Double -> Double -> String -> Bool
simpProp12 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Exponent (Var s) (Const 0)) == Const 1
testsimpProp12 = quickCheck simpProp12

{-| Tests that the derivative of variable where the string is defined in the string will just return the 
mulitplication of the value and -1 -}
simpProp13 :: Double -> Double -> String -> Bool
simpProp13 a b s = simplify (Map.fromList [(s,a),("y",b)]) (Neg (Var s)) == Mult (Const (-1)) (Const a)
testsimpProp13 = quickCheck simpProp13

{-| Tests that is sin is given a variable and the variable is defined in the dictionary it will return the 
the value as a constant-}
simpProp14 :: Double -> Double -> String -> Bool
simpProp14 a b s = simplify (Map.fromList [(s,a),("y",b)]) (Sin (Var s)) == Const (sin(a))
testsimpProp14 = quickCheck simpProp14

{-| Tests that is sin is given a variable and the variable is not defined in the dictionary it will return the value as is-}
simpProp15 :: Double -> Double -> String -> Bool
simpProp15 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Sin (Var s)) == Sin (Var s)
testsimpProp15 = quickCheck simpProp15

{-| Tests that is cos is given a variable and the variable is defined in the dictionary it will return the 
the value as a constant-}
simpProp16 :: Double -> Double -> String -> Bool
simpProp16 a b s = simplify (Map.fromList [(s,a),("y",b)]) (Cos (Var s)) == Const (cos(a))
testsimpProp16 = quickCheck simpProp16

{-| Tests that is cos is given a variable and the variable is not defined in the dictionary it will return the value as is-}
simpProp17 :: Double -> Double -> String -> Bool
simpProp17 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Cos (Var s)) == Cos (Var s)
testsimpProp17 = quickCheck simpProp17

{-| Tests the simplifcation of 2 constants will just return a them added together wrapped as a const -}
simpProp18 :: Double -> Double -> String -> Bool
simpProp18 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Add (Const a) (Const b)) == Const (a+b)
testsimpProp18 = quickCheck simpProp18

{-| Tests the simplifcation of a constant and a var is defined in the dictionary will just return a them added together wrapped as a const  -}
simpProp19 :: Double -> Double -> String -> Bool
simpProp19 a b s = simplify (Map.fromList [(s,a),("y",b)]) (Add (Var s) (Const b)) == Const (a+b)
testsimpProp19 = quickCheck simpProp19

{-| Tests the simplifcation of a constant and a var is not defined in the dictionary will just return the expression as is-}
simpProp20 :: Double -> Double -> String -> Bool
simpProp20 a b s = simplify (Map.fromList []) (Add (Var s) (Const b)) == Add (Var s) (Const b)

{-| Tests the simplifcation of 2 constants will just return a them multiplied together wrapped as a const -}
simpProp21 :: Double -> Double -> String -> Bool
simpProp21 a b s = simplify (Map.fromList [("x",a),("y",b)]) (Mult (Const a) (Const b)) == Const (a*b)
testsimpProp21 = quickCheck simpProp21

{-| Tests the simplifcation of a constant and a var is defined in the dictionary will just return a them multiplied together wrapped as a const  -}
simpProp22 :: Double -> Double -> String -> Bool
simpProp22 a b s = simplify (Map.fromList [(s,a),("y",b)]) (Mult (Var s) (Const b)) == Const (a*b)
testsimpProp22 = quickCheck simpProp22

{-| Tests the simplifcation of a constant and a var is not defined in the dictionary will just return the expression as is-}
simpProp23 :: Double -> Double -> String -> Bool
simpProp23 a b s = simplify (Map.fromList []) (Mult (Var s) (Const b)) == Mult (Var s) (Const b)

{-| Tests that simplify (Map.fromList []) (Mult (Cos (Mult (Var "x") (Const 4))) (Var "y")) returns (Mult (Cos (Mult (Var "x") (Const 4))) (Var "y"))-}
simpProp25 :: Bool
simpProp25 = simplify (Map.fromList []) (Mult (Cos (Mult (Var "x") (Const 4))) (Var "y")) == (Mult (Cos (Mult (Var "x") (Const 4))) (Var "y"))

{-| Tests that simplify (Map.fromList [("x",2),("y",4)]) (Mult (Mult (Var "x") (Const 4)) (Var "y")) returns Const 32.0-}
simpProp26 :: Bool
simpProp26 = simplify (Map.fromList [("x",2),("y",4)]) (Mult (Mult (Var "x") (Const 4)) (Var "y")) == Const 32.0

{-| Tests that z=xy, x=t^2, y=2t multi-variable differentiated-}
multDiffProp1 :: String -> String -> String -> Bool
multDiffProp1 x y t = multDiff (Mult (Var x) (Var y)) (Exponent (Var t) (Const 2)) (Mult (Const 2) (Var t)) x y t == (Add (Mult (partDiff x (Mult (Var x) (Var y))) (partDiff t (Exponent (Var t) (Const 2)))) (Mult (partDiff y (Mult (Var x) (Var y))) (partDiff t (Mult (Const 2) (Var t))) ) )
testmultDiffProp1 = quickCheck multDiffProp1

{-| Tests that anti-derivative of a constant-}
antiDerivProp1 :: Double -> Bool
antiDerivProp1 a = antiDeriv (Const a) == Mult (Const a) (Var "x")
testantiDerivProp1 = quickCheck antiDerivProp1

{-| Tests that anti-derivative of c^b, where c and b are constants-}
antiDerivProp2 :: Double -> Double -> Bool
antiDerivProp2 a b = antiDeriv (Exponent (Const a) (Const b)) == Mult (Exponent (Const a) (Const b)) (Var "x")
testantiDerivProp2 = quickCheck antiDerivProp2

{-| Tests that anti-derivative of x^c, where x is a variable and c is a constant-}
antiDerivProp3 :: Double -> String -> Bool
antiDerivProp3 a s = antiDeriv  (Exponent (Var s) (Const a)) == Mult (Exponent (Var s) (Const (a+1))) (Exponent (Const (a+1)) (Const (-1)))
testantiDerivProp3 = quickCheck antiDerivProp3

{-| Tests that anti-derivative of a 1/x-}
antiDerivProp4 :: String -> Bool
antiDerivProp4 s = antiDeriv (Exponent (Var s) (Const (-1.0))) == Ln (Var s)
testantiDerivProp4 = quickCheck antiDerivProp4

{-| Tests that anti-derivative of e^x, where x is a variable-}
antiDerivProp6 ::  String -> Bool
antiDerivProp6 s = antiDeriv (Exp (Var s)) == (Exp (Var s))
testantiDerivProp6 = quickCheck antiDerivProp6

{-| Tests that anti-derivative of e^c, where c is a variable-}
antiDerivProp7 ::  Double -> Bool
antiDerivProp7 a = antiDeriv (Exp (Const a)) == Mult (Exp (Const a)) (Var "x")
testantiDerivProp7 = quickCheck antiDerivProp7

{-| Tests that anti-derivative of sin x, where x is a variable-}
antiDerivProp8 ::  String -> Bool
antiDerivProp8 s = antiDeriv (Sin (Var s)) == Neg (Cos (Var s))
testantiDerivProp8 = quickCheck antiDerivProp8

{-| Tests that anti-derivative of cos x, where x is a variable-}
antiDerivProp9 ::  String -> Bool
antiDerivProp9 s = antiDeriv (Cos (Var s)) == Sin (Var s)
testantiDerivProp9 = quickCheck antiDerivProp9

{-| Tests that anti-derivative of sin c, where c is a constant-}
antiDerivProp10 ::  Double -> Bool
antiDerivProp10  a = antiDeriv (Sin (Const a)) == Mult (Sin (Const a)) (Var "x")
testantiDerivProp10 = quickCheck antiDerivProp10

{-| Tests that anti-derivative of cos c, where c is a constant-}
antiDerivProp11 ::  Double -> Bool
antiDerivProp11  a = antiDeriv (Cos (Const a)) == Mult (Cos (Const a)) (Var "x")
testantiDerivProp11 = quickCheck antiDerivProp11

{-| Tests that anti-derivative of ln c, where c is a constant-}
antiDerivProp12 ::  Double -> Bool
antiDerivProp12  a = antiDeriv (Ln (Const a)) == Mult (Ln (Const a)) (Var "x")
testantiDerivProp12 = quickCheck antiDerivProp12


{-Since quickcheck will test on string that are -}
{-| Tests to see how "x+y" will be parsed, where x and y are strings-}
parseProp1 :: String -> String -> Bool
parseProp1 x y = parseExprI (x ++ "+" ++ y) == Add (Var x) (Var y) 

{-| Tests to see how "x*y" will be parsed, where x and y are strings-}
parseProp2 :: String -> String -> Bool
parseProp2 x y = parseExprI (x ++ "*" ++ y) == Mult (Var x) (Var y) 

{-| Tests to see how "x^y" will be parsed, where x and y are strings-}
parseProp3 :: String -> String -> Bool
parseProp3 x y = parseExprI (x ++ "^" ++ y) == Exponent (Var x) (Var y) 

{-| Tests to see how "ey" will be parsed, where x and y are strings-}
parseProp4 ::  String -> Bool
parseProp4 y = parseExprIG ("e" ++ y) == Exp (Var y)

{-| Tests to see how "siny" will be parsed, where x and y are strings-}
parseProp5 ::  String -> Bool
parseProp5 y = parseExprIG ("sin" ++ y) == Sin (Var y)

{-| Tests to see how "cosy" will be parsed, where x and y are strings-}
parseProp6 ::  String -> Bool
parseProp6 y = parseExprIG ("cos" ++ y) == Cos (Var y)

{-| Tests to see how "a+b" will be parsed, where a and b are numbers-}
parseProp8 :: Double -> Double -> Bool
parseProp8 a b = parseExprD (show a ++ "+" ++ show b) == Add (Const a) (Const b)

{-| Tests to see how "a*b" will be parsed, where a and b are numbers-}
parseProp9 :: Double -> Double -> Bool
parseProp9 a b = parseExprD (show a ++ "*" ++ show b) == Mult (Const a) (Const b)

{-| Tests to see how "a^b" will be parsed, where a and b are numbers-}
parseProp10 :: Double -> Double -> Bool
parseProp10 a b = parseExprD (show a ++ "^" ++ show b) == Exponent (Const a) (Const b)

{-| Tests to see how "eb" will be parsed, where b is a number-}
parseProp11 ::  Double -> Bool
parseProp11 a = parseExprDG ("e" ++ show a) == Exp (Const a)

{-| Tests to see how "sinb" will be parsed, where b is a number-}
parseProp12 ::  Double -> Bool
parseProp12 a = parseExprDG ("sin" ++ show a) == Sin (Const a)

{-| Tests to see how "cosb" will be parsed, where b is a number-}
parseProp13 ::  Double -> Bool
parseProp13 a = parseExprDG ("cos" ++ show a) == Cos (Const a)
