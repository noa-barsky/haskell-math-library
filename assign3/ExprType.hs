{--|
Module : ExprType
Description: Contains a typeclass definition which defines some built-in functions specific to the Expr type, as well as a getvars function to retrieve all the variables from an expression
Copyright: (c) Noa Barsky @2018
License : WTFPL 
Maintainer : noabarsky123[at]gmail[dot]com
Stability : experimental
Portability : POSIX
-}

module ExprType where

import           Data.List

{-This data type allows for all basic expressions to be represented. Notice there is no division, but there are exponents, so simply
making the power negative, division would be represented. No need for subtraction, when there is a symbolic values for 
negatives and addition. The Log function is left out of the type as this is a library intended to help with first year calculus
and it is rare to use log, ln is more standard in math.-}
data Expr a = Add (Expr a) (Expr a) -- ^ This constructur takes two expressions of type 'Expr a' and wraps them in an Add Constructor, to represent addition
            | Mult (Expr a) (Expr a) -- ^ This constructur takes two expressions of type 'Expr a' and wraps them in an Mult Constructor, to represent multiplication
            | Const a -- ^ This constructor represents a constant of some a number value
            | Var String -- ^ This constructor represents a variable as some string
            | Cos (Expr a) -- ^ This constructor represents a cos function and it wraps some expression of the 'Expr a' type
            | Sin (Expr a) -- ^ This constructor respresents a sin function and it wraps some expression of the 'Expr a' type
            | Ln (Expr a) -- ^ This constructor respresents a ln function and it wraps some expression of the 'Expr a' type
            | Exp (Expr a) -- ^ This constructor represents the e function and it wraps some expression of the 'Expr a' type
            | Neg (Expr a) -- ^ This constructor represents a negative value and wraps some expression of the 'Expr a' type
            | Exponent (Expr a) (Expr a) -- ^ This constructor represents an exponent, the first 'Expr a' value being the base of the exponent and the the second being the power
  deriving (Eq)

{-This type definition is used to symbolically represent commone trig values. Although
Haskell has built-in function which use trig lookup tables find the values. Although,
these values are accurate, they displayed as decimals and generally speaking in math
the symbolic version of these values are given-}

{-In the constructor Frac, the first Trig value is the numerator and the second in the denominator-}
data Trig = Frac (Trig) (Trig) -- ^ This constructor represents a fraction with the first Trig value being the numerator and the second being the denominator
          | Sqrt (Trig) -- ^ This constructor represents the square root of a value
          | Zero -- ^This constructor is the symbolic 0
          | One -- ^This constructor is the symbolic 1
          | Two -- ^This constructor is the symbolic 2
          | Three -- ^This constructor is the symbolic 3
          | NegT (Trig) -- ^This constructor is the symbolic negative for trig functions
          | Infinity -- ^This constuctor is the symbolic representation of infinity
  deriving Show

{-| getVars function retrieves the variables from an Expr type.-}
getVars :: Expr a -> [String] {-This function is useful in the sense that if you have a long expression and want to know all
                                the variables you need to asssign value to.-}
getVars (Const _)    = [] {-Const, implying there are no variables, so will always return an empty list-}
getVars (Var x)      = [x] 
getVars (Exp e)      = getVars e
getVars (Neg e)      = getVars e
getVars (Ln e)       = getVars e
getVars (Cos e)      = getVars e
getVars (Sin e)      = getVars e
getVars (Add e1 e2)  = getVars e1 `union` getVars e2 {-gets the values of each expression and then concatinates the two lists of values-}
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Exponent e1 e2)  = getVars e1 `union` getVars e2


