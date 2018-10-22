{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{--|
Module : ExprDiff
Description: Contains a type class definition for the class DiffExpr as well as some defintion to some built-in functions for this class
Copyright: (c) Noa Barsky @2018
License : WTFPL 
Maintainer : noabarsky123[at]gmail[dot]com
Stability : experimental
Portability : POSIX
-}


module ExprDiff where

import           ExprType

import qualified Data.Map.Strict as Map

{-This module defines a type class for the type definition Expr a, within this class definition there are several "built-in" functions.
 Below that is an instance built around the Floating class type, as such all values of these functions will be returned Floating types.-}

class DiffExpr a where
  {-eval,simplify,parDiff,simplifiable,multDiff, and antiDerive all have their type declarations declared here and the function defined below.-}
  {- | eval function, a function that given a dictionary and an expression of the type 'Expr a' will evaluate it as something of the 'Floating' class
  Example: @eval (Map.fromList [("x",3),("y",4)]) (Add (Var "x") (Var "y")) => 7.0 @
  -}
  eval :: Map.Map String a -> Expr a -> a
  {- | simplify function, a function that given something of the 'Expr' type and a dictionary will simplify it to as close to as normal form as possible 
  and return it back as an 'Expr a' type. Example: @simplify (Map.fromList [("x",5),("y",2)]) (Add (Add (Var "x") (Var "y")) (Mult (Var "y") (Var "z"))) 
  => ((val 7.0)) + (((val 2.0)) * ((var "z")))@
  -}
  simplify :: Map.Map String a -> Expr a -> Expr a --Map.Map String a is a dictionary made of a list of tuples are of the type [(String, Num)] (num being the class defintion, so any type of number will work)
  {-| partDiff function, a function which takes in a string, which represents which variable the function should derived with respect to 
  and an expression of the 'Expr a' type the the function will differentiate the expression symbolically, parially or fully. 
  Example: @partDiff "x" (Mult (Var "x") (Var "y")) =>  (((val 1.0)) * ((var "y"))) + (((var "x")) * ((val 0.0)))@-} 
  partDiff :: String -> Expr a -> Expr a
  {- | multDiff function, a function with will symbolically takes the multi-variant derivative of some function with respect to two variables 
  and those two variables with respect to some other one variable
  Example: @ simplify (Map.fromList []) (multDiff (Mult (Var "x") (Var "y")) (Exponent (Var "t") (Const 2)) (Mult (Const 2) (Var "t")) "x" "y" "t")
  => (((((val 1.0)) * ((var "y"))) + (((var "x")) * ((val 0.0)))) * ((((val 2.0)) * (((var "t")) ^ (((val 2.0)) + ((val -1.0))))) * 
  (((var "y")) * (((val 2.0)) * (((var "t")) ^ ((val 1.0))))) + (((var "x")) * ((val 2.0))) -}
  multDiff :: Expr a -> Expr a -> Expr a -> String -> String -> String -> Expr a
  {- | antiDeriv function, a function that given certain expressions will output the anti-derivative of the function.
  Example: @antiDeriv (Exponent (Var "x") (Const (-1))) => (ln ((var "x")))@ -}
  antiDeriv :: Expr a -> Expr a
  -- | simplifiable function, a function that simply checks if the expression is already in simplified form 
  simplifiable :: Map.Map String a -> Expr a -> Bool
  

  {-The functions below can be thought of as applying typical operators such as +,*,**, etc over Expr types and returning the value wrapped as Expr types-}
  -- | A function which will convert !+ into Add e1 e2 
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) (Add e1 e2) {-Example input: would be (Const 3) !+ (Const 4) would output => (Const 7.0) -}
  -- | A function which will apply multiplication to a Expr type and return it wrapped as an Expr type
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) (Mult e1 e2)
  -- | A function which will convert some x into (Const (-x))
  neg :: Expr a -> Expr a
  neg x = Const (eval (Map.fromList []) (Neg x)) {-Example input: neg (Const 3) => Const (-3)-}
  -- | A function which will convert some number base b and some number power x into Const b^x
  (!^) :: Expr a -> Expr a -> Expr a
  b !^ x = simplify (Map.fromList []) (Exponent b x) {-Example input: (Const 2) !^ (Const 3) => Const 8.0 -}
  -- | A function which will some number x into 'Const x'
  val :: a -> Expr a
  val x = Const x {-Example input: val 4 => Const 4.0-}
  -- | A function which will convert some string x into 'Var "x"'
  var :: String -> Expr a
  var x = Var x {-Example input: var "x" => Var "x"-}
  -- | A function which will convert some x into 'Cos x'
  myCos :: Expr a -> Expr a 
  myCos x = Cos x {-Example input: myCos (Const 6.0) => Cos (Const 6.0)-}
  -- | A function which will convert some x into 'Sin x'
  mySin :: Expr a -> Expr a 
  mySin x = Sin x {-Example input: mySin (Const 6.0) => Sin (Const 6.0)-}
  -- | A function which will convert some x into 'Ln x'
  myLn :: Expr a -> Expr a 
  myLn x = Ln x {-Example input: myLn (Const 6) => Ln (Const 6.0)-}
  -- | A function which will convert some x into e^x
  myExp :: Expr a -> Expr a 
  myExp x = Exp x {-Example input: myExp (Const 6) => Exp (6.0)-}
  

{-The following is an instance for Floating class numbers for the class definition DiffExpr,
 within it is the function definitions for eval, partDiff, simplify, multDiff, and antiDeriv-}
instance (Floating a, Eq a) => DiffExpr a where
  {-This is the function definition of eval and this essentially takes something of the type Expr and returns a floating point number. 
  This is especially useful as some of the below functions have long outputs and it's easier to understand them when they are evaluated 
  with respect to some variables. These are pattern matching for eval, where this function takes a dictionary, which is a list of tuples 
  of the type (String, Num)-}
  eval vrs (Const x) = x 
  eval vrs (Var x) = case Map.lookup x vrs of {-This unwraps values in the maybe type and throw an error is 
                                                the variable in the dictionary does not match a variable in the expression -}
                       Just v  -> v
                       Nothing -> error "Failed to find a matching variable in the expression and dictionary"
  {-Essentially, this takes the symbolic operators and adds real ones to evaluate expressions down to a decimal number-}
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2 
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Neg e) = (-1) * (eval vrs e) {-Simply makes things negative-}
  eval vrs (Exp e) = exp (eval vrs e)
  eval vrs (Exponent b x) = (eval vrs b) ** (eval vrs x)
  {-cos, sin, and log are all built-in haskell functions which evaluation cosine and sine values using correct values, but as floating point numbers.
  And log simply outputs the ln value of what is being evaluated and returns the floating value.-}
  eval vrs (Cos e) = cos (eval vrs e)
  eval vrs (Sin e) = sin (eval vrs e)
  eval vrs (Ln e) = log (eval vrs e)
  
  {-This function partially or fully differentiates a function of the Expr type by specifying that the variable the is being differentiated with 
  respect to is a parameter. If the variable specified is not in the expression then it will be evaluated to (Const 0.0).-}
  partDiff s (Var x) | x == s = (Const 1) {-Differentiates partially, so any variable that is not the specified one in the parameter then the variable is 
                                             treated as a constant and evaluated down to (Const 0.0)-}
                     | otherwise = (Const 0)
  partDiff _ (Const _) = Const 0 {-Differentiares constants, so not matter the value it was always returns a 0 for its derivative-}
  partDiff s (Add e1 e2) = Add (partDiff s e1) (partDiff s e2) {-Differentiates expressions being added together-}
  partDiff s (Mult e1 e2) = Add (Mult (partDiff s e1) e2) (Mult e1 (partDiff s e2)) {-Differentiates expressions being multiplied together, so applies chain rule-}
  partDiff s (Ln e) = Mult (Exponent e (Const (-1))) (partDiff s e) {-Differentiates ln expressions, using chain rule-}
  partDiff s (Exponent (Const a) e1) = Mult (Mult ((Exponent (Const a) e1)) (Ln (Const a))) (partDiff s e1) {-Differentiates an expression where 
                                                                                                the base is a constant it is being raised by a variable-}
  partDiff s (Exponent b x) = Mult (Mult x (Exponent b (Add x (Const (-1))))) (partDiff s b) {-Diffentiaties an expression where the base is a variable and the exponent is some expression-}
  partDiff s (Sin x) = Mult (Cos x) (partDiff s x) {-Differentiates the cosine function, using chain rule-}
  partDiff s (Cos x) = Mult (Neg (Sin x)) (partDiff s x) {-Differentiates the sine function, using chain rule-}
  partDiff s (Exp e) = Mult (Exp e) (partDiff s e) {-Differentiates e, using chain rule-}
  
  
  {-This function simplifies Expr types to as close to normal form as possible, since getting to normal form is a complex process it will not
  simplify any expression to it fullest and can only simplfiy to a certain point.-}

  {-This function calls the eval functio and then wraps the values in an expression type. It aims to simplify expressions like 
  Add (Add (Const 5) (Const 6)) (Const 2) => Const 13.0; however, it can also simplify things like 
  Add (Add (Const 5) (Const 6)) (Var "y") => Add (Const 11) (Var "y")-}
  simplifiable vrs e1 = (simplify vrs e1) /= e1

  {-General simplication, mostly used to refer back to in later code-}
  simplify vrs (Const a) = Const a
  simplify vrs (Neg e1) = Mult (Const (-1)) (simplify vrs e1)
  simplify vrs (Var x) = case Map.lookup x vrs of {-Instead of throwing an error in a variable isn't in a dictionary, this just returns the variable back,
                                                    the reason for this being that something like 
                                                    simplify (Map.fromList []) Add (Var "x") (Add (Const 5) (Const 9)) Evaluated down to 
                                                    Add (Var "x") (Const 14.0) rather than throwing an error -}
                           Just v -> Const (eval vrs (Var x))
                           Nothing -> Var x 
  {-Some Shortcuts, just some basic shortcute that outline some identity properties of some of
    the operations-}
  simplify vrs (Mult (Const 0) _ ) = Const 0
  simplify vrs (Mult _ (Const 0)) = Const 0
  simplify vrs (Mult (Const 1) e1 ) = simplify vrs e1
  simplify vrs (Mult e1 (Const 1)) = simplify vrs e1
  simplify vrs (Add (Const 0) e1) = simplify vrs e1
  simplify vrs (Add e1 (Const 0)) = simplify vrs e1
  simplify vrs (Ln (Const 1)) = Const 0
  simplify vrs (Exp (Const 0)) = Const 1
  simplify vrs (Exponent _ (Const 0)) = Const 1 

  {-handles how to simplify sin when given expr type const, which is just to evaluate it and then wrap the value as a constant-}
  simplify vrs (Sin (Const a)) = Const (eval vrs (Sin (Const a)))
  {-handles how to deal with all other cases, so if it's not the dictionary, then don't simplify it down to a constant
   The simplification is called because is the variable is not in the dictionary and simplify is called on the 
   function again then you run into the there is infinite recursion. So simplification just varifies that it is not
   already as simplified as possible. -}
  simplify vrs (Sin e1) = case (simplifiable vrs e1) of
                              False -> (Sin (simplify vrs e1))
                              True -> simplify vrs (Sin (simplify vrs e1))

  {-Ln and cos follow the exact pattern as Sin, but replaced with Ln and Cos-}                            
  simplify vrs (Cos (Const a)) = Const (eval vrs (Cos (Const a)))
  simplify vrs (Cos e1) = case (simplifiable vrs e1) of
                              False -> (Cos (simplify vrs e1))
                              True -> simplify vrs (Cos (simplify vrs e1))

   
  simplify vrs (Ln (Const a)) = Const (eval vrs (Ln (Const a)))
  simplify vrs (Ln e1) = case (simplifiable vrs e1) of
                            False -> (Ln (simplify vrs e1))
                            True -> simplify vrs (Ln (simplify vrs e1))

  {-Defines how add should react when given to constants-}                          
  simplify vrs (Add (Const a) (Const b)) = Const (eval vrs (Add (Const a) (Const b)))
  {-associatively assigns how add should react if given two vars, by checking
    if both variables are in the dicionary to see if it should 
      evaluated as normal. Otherwise, it simplifies as much as possible-}
  simplify vrs (Add (Var x) (Var y)) = case (Map.lookup x vrs) of
                                          Just v -> case (Map.lookup y vrs) of
                                                      Just t -> Const (eval vrs (Add (Var x) (Var y)))
                                                      Nothing -> Add (simplify vrs (Var x)) (simplify vrs (Var y))
                                          Nothing -> Add (simplify vrs (Var x)) (simplify vrs (Var y))
  {-This deals with how add should react when add is given a var and a const-}
  simplify vrs (Add (Var x) (Const a)) = case Map.lookup x vrs of
                                          Just v -> Const (eval vrs (Add (Var x) (Const a)))
                                          Nothing -> Add (Var x) (Const a)
  {-This just creates associativity, so it does not matter which order the const and var go in-}
  simplify vrs (Add (Const a) (Var x)) = case Map.lookup x vrs of
                                           Just v -> Const (eval vrs (Add (Var x) (Const a)))
                                           Nothing -> Add (Const a) (Var x) 
  {-check if the function is already simplified or not
    and if not, that way it can further simplify it if not
    it's not simplified and if it then it will just simplify the
    expressions, no the add expression overall and not cause
    infinite recursion-}
  simplify vrs (Add e1 e2) = case ((simplifiable vrs e1) || (simplifiable vrs e2)) of
                                 False -> (Add (simplify vrs e1) (simplify vrs e2))
                                 True -> simplify vrs (Add (simplify vrs e1) (simplify vrs e2))
  
  {-This is how Mult is simplified, it follows the same pattern and logic and as Add-}
  simplify vrs (Mult (Const a) (Const b)) = Const (eval vrs (Mult (Const a) (Const b)))
  simplify vrs (Mult (Var x) (Var y)) = case (Map.lookup x vrs) of
                                          Just v -> case (Map.lookup y vrs) of
                                                      Just t -> Const (eval vrs (Mult (Var x) (Var y)))
                                                      Nothing -> Mult (simplify vrs (Var x)) (simplify vrs (Var y))
                                          Nothing -> Mult (simplify vrs (Var x)) (simplify vrs (Var y))
  simplify vrs (Mult (Var x) (Const a)) = case Map.lookup x vrs of
                                           Just v -> Const (eval vrs (Mult (Var x) (Const a)))
                                           Nothing -> Mult (Var x) (Const a)
  simplify vrs (Mult (Const a) (Var x)) = case Map.lookup x vrs of
                                             Just v -> Const (eval vrs (Mult (Var x) (Const a)))
                                             Nothing -> Mult (Const a) (Var x) 
  simplify vrs (Mult e1 e2) = case  ((simplifiable vrs e1) || (simplifiable vrs e2)) of
                                          False -> (Mult (simplify vrs e1) (simplify vrs e2))
                                          True -> simplify vrs (Mult (simplify vrs e1) (simplify vrs e2))

  {-This is how Exponent is simplified, it follows the same pattern and logic and as Add-}
  simplify vrs (Exponent (Const a) (Const b)) = Const (eval vrs (Exponent (Const a) (Const b)))
  simplify vrs (Exponent (Var x) (Var y)) = case (Map.lookup x vrs) of
                                          Just v -> case (Map.lookup y vrs) of
                                                      Just t -> Const (eval vrs (Exponent (Var x) (Var y)))
                                                      Nothing -> Exponent (simplify vrs (Var x)) (simplify vrs (Var y))
                                          Nothing -> Exponent (simplify vrs (Var x)) (simplify vrs (Var y)) 
  simplify vrs (Exponent (Var x) (Const a)) = case Map.lookup x vrs of
                                                Just v -> Const (eval vrs (Exponent (Var x) (Const a)))
                                                Nothing -> Exponent (Var x) (Const a)
  simplify vrs (Exponent (Const a) (Var x)) = case Map.lookup x vrs of
                                                Just v -> Const (eval vrs (Exponent (Var x) (Const a)))
                                                Nothing -> Exponent (Const a) (Var x) 
  simplify vrs (Exponent e1 e2) = case  ((simplifiable vrs e1) || (simplifiable vrs e2)) of
                                         False -> (Exponent (simplify vrs e1) (simplify vrs e2))
                                         True -> simplify vrs (Exponent (simplify vrs e1) (simplify vrs e2))
  

  {-This will display the multi-variant chain rule as an Expr type for some funciton ez with respect to ex and ey, 
  with ex and ey with respect to t.-}                               
  multDiff ez ex ey x y t = Add (Mult (partDiff x ez) (partDiff t ex)) (Mult (partDiff y ez) (partDiff t ey))

  {-This function simply define anti-derivative rules for certain expressions. It displays it
  as a Expr type. -}
  antiDeriv (Const a) = Mult (Const a) (Var "x")
  antiDeriv (Exponent (Const a) (Const b)) = Mult (Exponent (Const a) (Const b)) (Var "x")
  antiDeriv (Exponent (Var x) (Const (-1))) = Ln (Var x)
  antiDeriv (Exponent (Var x) (Const n)) = Mult (Exponent (Var x) (Const (n+1))) (Exponent (Const (n+1)) (Const (-1)))
  antiDeriv (Exp (Var x)) = (Exp (Var x))
  antiDeriv (Exp (Const a)) = Mult (Exp (Const a)) (Var "x")
  antiDeriv (Sin (Var x)) = Neg (Cos (Var x))
  antiDeriv (Cos (Var x)) = Sin (Var x)
  antiDeriv (Sin (Const a)) = Mult (Sin (Const a)) (Var "x")
  antiDeriv (Cos (Const a)) = Mult (Cos (Const a)) (Var "x")
  antiDeriv (Ln (Const a)) = Mult (Ln (Const a)) (Var "x")



