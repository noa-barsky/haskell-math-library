{--|
Module : ExprPretty
Description: Contains a way to make things looks nicer and put them into easy to read strings
Copyright: (c) Noa Barsky @2018
License : WTFPL 
Maintainer : noabarsky123[at]gmail[dot]com
Stability : experimental
Portability : POSIX
-}

module ExprPretty where

import           ExprType

{-| Simply puts brackets around strings-}
parens :: String -> String
parens ss = "(" ++ ss ++ ")"

{-| Creates an instance of the typeclass show to displays Expr types as easier to read strings-}
instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parens (show e1) ++ " * " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " + " ++ parens (show e2)
  show (Exponent e1 e2)  = parens (show e1) ++ " ^ " ++ parens (show e2)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x
  show (Cos e1)     = parens $ "cos " ++ parens (show e1)
  show (Sin e1)     = parens $ "sin " ++ parens (show e1)
  show (Ln e1)     = parens $ "ln " ++ parens (show e1)
  show (Exp e1)     = parens $ "e " ++ parens (show e1)
  show (Neg e1)     = parens $ "- " ++ parens (show e1)
  