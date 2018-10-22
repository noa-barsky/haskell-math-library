{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{--|
Module : ExprTrig
Description: Contains and dictionary with common trig angle identities and a functions to map values from that dictionary
Copyright: (c) Noa Barsky @2018
License : WTFPL 
Maintainer : noabarsky123[at]gmail[dot]com
Stability : experimental
Portability : POSIX
-}

module ExprTrig (evalTrig, trigLookUp) where

import           ExprType
import           ExprDiff

import qualified Data.Map.Strict as Map


{-The purpose of this module is simply for convience. You can type in a certain string and get the special triangle value
of the string. The input should be a string formatted like this: "sin pi/3". Although the input could have been of the (Expr a) type
it less convient to type out and the purpose of this module is to quickly retrieve common values. It only defines value for common
trig identities (special triangles and common cosine/sine graph angles) because these are the trig values I was mostly working
with in my calculus classes. -}



{- |This function will return a symbolic value given a string that that specifies cos or sin and a common trig angle.
if the value is not a common trig angle (i.e. an angle in a special triangle or from 0-2pi) then an error will thrown
Example: "sin pi/6" => (Frac (One) (Two))-}

{-This function simply looks the string in the trigLookUp trig and gives an error message if the value is not in the dictionary.-}
evalTrig :: String -> Trig
evalTrig ss = case Map.lookup ss trigLookUp of
               Just ss -> ss
               Nothing -> error "Your input was not a common trig value"

{- |This is simply a dicionary that uses strings with cos or sin and their respective angles, and mapes it to a 'Trig' type. -}


{-Simply a dictionary that assigns string keys to their respective trig values of the Trig type-}
trigLookUp :: Map.Map String Trig
trigLookUp = Map.fromList [("sin pi/6",(Frac (One) (Two))) {-Frac, where the first value in the numerator and the second value is the denominator-}
                          ,("sin pi/3",(Frac (Sqrt (Three)) (Two)))
                          ,("sin pi/4",(Frac (One) (Sqrt (Two))))
                          ,("sin pi/2", One)
                          ,("sin pi", Zero)
                          ,("sin 0", Zero)
                          ,("sin 3pi/2", NegT (One))
                          ,("sin 2pi", Zero)
                          ,("cos pi/6",(Frac (Sqrt (Three)) (Two))) 
                          ,("cos pi/3",(Frac (One) (Two)))
                          ,("cos pi/4",(Frac (One) (Sqrt (Two))))
                          ,("cos pi/2", Zero)
                          ,("cos pi", NegT (One))
                          ,("cos 0", One)
                          ,("cos 3pi/2",Zero)
                          ,("cos 2pi", One)
                          ,("tan pi/6",(Frac (One) (Sqrt (Three))))
                          ,("tan pi/3",(Sqrt (Three)))
                          ,("tan pi/4",One)
                          ,("tan pi/2", Infinity) 
                          ,("tan pi", Zero)
                          ,("tan 0", Zero)
                          ,("tan 2pi", Zero)]
                          
