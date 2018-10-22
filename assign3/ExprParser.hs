{--|
Module : ExprParser
Description: Contains a parsers to parse string into Expr types
Copyright: (c) Noa Barsky @2018
License : WTFPL 
Maintainer : noabarsky123[at]gmail[dot]com
Stability : experimental
Portability : POSIX
-}


module ExprParser (parseExprI, parseExprIG, parseExprD, parseExprDG,parseExprInt,parseExprIntG,parseExprF,parseExprFG) where


import           ExprType

import           Text.Parsec
import           Text.Parsec.String

{-This module uses the Parsec library to parse certain strings in Expr a types.
Not all strings can be parsed and the format of the string to be parsed is 
detailed below. The module's parsers are divided into parsers which parse: Integer, Int, Double, and Float. 
As of right now expression with operators (+,*,^) cannot be parsed with functions such as (e,sin,cos,ln). As well
order of operations will not be perserved, hoping to change in future -}

-- ** Main Parsers
{- | This parser, parses numbers represented as strings into the Integer type of an Expr constructor of the type Add, Mult, Exponent, or Const 
depending on what operators (+,*,^) are present in the string and returns an error message if there aren't any numbers in the string. 
    Example: @parseExprI "4" => (val 4)@ If you try parsing a decimal or an expression with a decimal the function will stop parsing once it
     reaches the decimal value. If you wish to subtract two statements, remember the Expr type does not have a subtract constructor, 
     so this must be done using add and making expressions negative. so 4-5 should be inputted as @parseExprI "4+-5"@. You can add padding 
     it the expression, so 4+5 will be return the exact same as 4 + 5 This parser can also parse letters as variables. 
     Example: @parseExprI "x+y" => ((var "x")) + ((var "y"))@. Note: This parser will not preserve order of operations and 
     can only parse the following operations:+,*,^.
-}


{-This parser specifically parses strings as integers and only excepts numbers as strings as input
Example input: parseExprI "123" => Const 123, parseExprI "1.56" => Const 1-}
parseExprI :: String -> Expr Integer 
parseExprI ss = case parse setexprI  "" ss of
                Left err  -> error "This is invalid input for parsing integers"
                Right expr -> expr 


{-This function essentialy "chains" the term to setOpMain, which basically assigns a Expr type given
what operation is present in the string-}
setexprI :: Parser (Expr Integer)
setexprI = (termI `chainl1` setOpMain) 
{-This function either returns the as a negative if there is a negative present
or it returns the factor as is, if not.-}
termI :: Parser (Expr Integer)
termI = (notOp factorI) <|> factorI 

{-Effectively, either parses the string as a number-> Const or a letter->Var-}
factorI :: Parser (Expr Integer)
factorI = try numParseI <|> varParse

{- | This parser, parses strings containing numbers into the Integer type of an Expr constructor of the type Exp, Ln, Cos, Sin depending on what 
    key words are present (e,ln,cos,sin) in the string and returns an error message if there aren't any letters a-z (lower or upper case) in the string. 
    Example: @parseExprIG "e4" => (e ((val 4)))@. Expressions can be padded with spaces so "e4" and "e 4" will return the same result. It can also 
    parse letters as variables. Example: @parseExprIG "ex" => (e ((var “x”)))@ Note: This parser will not preserve order of operations and can only
     parse the following functions: e,ln,cos,sin.
-}
parseExprIG :: String -> Expr Integer
parseExprIG ss = case parse setexprIG "" ss of
                Left err  -> error "This is invalid input for parsing variable strings"
                Right expr -> expr 

{-Since the constructors in opRest only have one parameter, `chainl` does not work in this case.
To compensate for this  is the following function. It set the variable op to something in the 
function setOpRest which is what parses the function: e,ln,cos, and sin. Then it tries to 
to parse the string as a function if that fail it will return the first thing it can parse. -}
setexprIG :: Parser (Expr Integer)
setexprIG = let
            opRest = do {op <- setOpRest;
                                    spaces;
                                    term <- termIG;
                                    spaces;
                                    return (op term)}
                in try opRest <|> termIG

termIG :: Parser (Expr Integer)
termIG = (notOp factorIG) <|> factorIG

factorIG :: Parser (Expr Integer)
factorIG = try numParseI <|> varParse

numParseI :: Parser (Expr Integer)
numParseI = do {i <- integer;
               return (Const i)}


{-Note: The rest of the main parsers work exactly the same as the two above, just on different number types.-}

{- | This parser, parses numbers represented as strings into the Int type of an Expr constructor of the type Add, Mult, Exponent, or Const 
depending on what operators (+,*,^) are present in the string and returns an error message if there aren't any numbers in the string. 
    Example: @parseExprInt "4" => (val 4)@ If you try parsing a decimal or an expression with a decimal the function will stop parsing once it
     reaches the decimal value. If you wish to subtract two statements, remember the Expr type does not have a subtract constructor, 
     so this must be done using add and making expressions negative. so 4-5 should be inputted as @parseExprInt "4+-5"@. You can add padding 
     it the expression, so 4+5 will be return the exact same as 4 + 5 This parser can also parse letters as variables. 
     Example: @parseExprInt "x+y" => ((var "x")) + ((var "y"))@. Note: This parser will not preserve order of operations and 
     can only parse the following operations:+,*,^.
-}
parseExprInt :: String -> Expr Int 
parseExprInt ss = case parse setexprInt  "" ss of
                 Left err  -> error "This is invalid input for parsing integers"
                 Right expr -> expr 
{-This function essentialy "chains" the term to setOpMain, which basically -}
setexprInt :: Parser (Expr Int)
setexprInt =  termInt `chainl1` setOpMain

termInt :: Parser (Expr Int)
termInt = (notOp factorInt) <|> factorInt 

factorInt :: Parser (Expr Int)
factorInt = try numParseInt <|> varParse

{- | This parser, parses strings containing numbers into the Int type of an Expr constructor of the type Exp, Ln, Cos, Sin depending on what 
    key words are present (e,ln,cos,sin) in the string and returns an error message if there aren't any letters a-z (lower or upper case) in the string. 
    Example: @parseExprIntG "e4" => (e ((val 4)))@. Expressions can be padded with spaces so "e4" and "e 4" will return the same result. It can also 
    parse letters as variables. Example: @parseExprIntG "ex" => (e ((var “x”)))@ Note: This parser will not preserve order of operations and can only
     parse the following functions: e,ln,cos,sin.
-}
parseExprIntG :: String -> Expr Int
parseExprIntG ss = case parse setexprIntG "" ss of
                Left err  -> error "This is invalid input for parsing variable strings"
                Right expr -> expr 

setexprIntG :: Parser (Expr Int)
setexprIntG = let
            opRest = do {op <- setOpRest;
                                    spaces;
                                    term <- termIntG;
                                    spaces;
                                    return (op term)}
                in try opRest <|> termIntG

termIntG :: Parser (Expr Int)
termIntG = (notOp factorIntG) <|> factorIntG

factorIntG :: Parser (Expr Int)
factorIntG = try numParseInt <|> varParse

numParseInt :: Parser (Expr Int)
numParseInt = do {i <- int;
               return (Const i)}



{- | This parser, parses numbers represented as strings into the Double type of an Expr constructor of the type Add, Mult, Exponent, or Const 
depending on what operators (+,*,^) are present in the string and returns an error message if there aren't any numbers in the string. 
    Example: @parseExprD "4" => (val 4).0@ If you try parsing a decimal or an expression with a decimal the 
    function will stop parsing once it reaches the decimal value. If you wish to subtract two statements, remember the
     Expr type does not have a subtract constructor, so this must be done using add and making expressions negative,
      so 4-5 should be inputted as @parseExprD "4+-5"@. You can add padding it the expression, so 4+5  
      will be return the exact same as 4 + 5 This parser can also parse letters as variables. 
      Example: @parseExprD "x+y" => ((var "x")) + ((var "y"))@.
       Note: This parser will not preserve order of operations and can only parse the following operations:+,*,^.
-}
parseExprD :: String -> Expr Double
parseExprD ss = case parse setexprD  "" ss of
                Left err  -> error "This is invalid input for parsing doubles"
                Right expr -> expr 

setexprD :: Parser (Expr Double)
setexprD = termD `chainl1` setOpMain

termD :: Parser (Expr Double)
termD = (notOp factorD) <|> factorD

factorD :: Parser (Expr Double)
factorD = try numParseD <|> varParse


{- | This parser, parses strings containing numbers into the Double type of an Expr constructor of the type Exp, Ln, Cos, Sin depending on what 
    key words are present (e,ln,cos,sin) in the string and returns an error message if there aren't any letters a-z (lower or upper case) in the string. 
    Example: @parseExprDG "e4" => (e ((val 4.0)))@. Expressions can be padded with spaces so "e4" and "e 4" will return the same result. It can also 
    parse letters as variables. Example: @parseExprDG "ex" => (e ((var “x”)))@ Note: This parser will not preserve order of operations and can only
     parse the following functions: e,ln,cos,sin.
-}
parseExprDG :: String -> Expr Double
parseExprDG ss = case parse setexprDG "" ss of
                Left err  -> error "This is invalid input for parsing variable strings"
                Right expr -> expr 

setexprDG :: Parser (Expr Double)
setexprDG = let
            opRest = do {op <- setOpRest;
                                    spaces;
                                    term <- termDG;
                                    spaces;
                                    return (op term)}
                in try opRest <|> termDG



termDG :: Parser (Expr Double)
termDG = (notOp factorDG) <|> factorDG

factorDG :: Parser (Expr Double)
factorDG = try numParseD <|> varParse

numParseD :: Parser (Expr Double)
numParseD = do {i <- double;
                return (Const i)}
 
        

{- | This parser, parses numbers represented as strings into the Float type of an Expr constructor of the type Add, Mult, Exponent, or Const 
depending on what operators (+,*,^) are present in the string and returns an error message if there aren't any numbers in the string. 
    Example: @parseExprF "4" => (val 4.0)@ If you try parsing a decimal or an expression with a decimal the 
    function will stop parsing once it reaches the decimal value. If you wish to subtract two statements, remember the
     Expr type does not have a subtract constructor, so this must be done using add and making expressions negative,
      so 4-5 should be inputted as @parseExprF "4+-5"@. You can add padding it the expression, so 4+5  
      will be return the exact same as 4 + 5 This parser can also parse letters as variables. 
      Example: @parseExprF "x+y" => ((var "x")) + ((var "y"))@.
       Note: This parser will not preserve order of operations and can only parse the following operations:+,*,^.
-}
parseExprF :: String -> Expr Float
parseExprF ss = case parse setexprF  "" ss of
                Left err  -> error "This is invalid input for parsing floats"
                Right expr -> expr 

setexprF :: Parser (Expr Float)
setexprF = termF `chainl1` setOpMain

termF :: Parser (Expr Float)
termF = (notOp factorF) <|> factorF

factorF :: Parser (Expr Float)
factorF = try numParseF <|> varParse


{- | This parser, parses strings containing numbers into the Float type of an Expr constructor of the type Exp, Ln, Cos, Sin depending on what 
    key words are present (e,ln,cos,sin) in the string and returns an error message if there aren't any letters a-z (lower or upper case) in the string. 
    Example: @parseExprFG "e4" => (e ((val 4.0)))@. Expressions can be padded with spaces so "e4" and "e 4" will return the same result. It can also 
    parse letters as variables. Example: @parseExprFG "ex" => (e ((var “x”)))@ Note: This parser will not preserve order of operations and can only
     parse the following functions: e,ln,cos,sin.
-}
parseExprFG :: String -> Expr Float
parseExprFG ss = case parse setexprFG "" ss of
                Left err  -> error "This is invalid input for parsing variable strings"
                Right expr -> expr 

setexprFG :: Parser (Expr Float)
setexprFG = let
            opRest = do {op <- setOpRest;
                                    spaces;
                                    term <- termFG;
                                    spaces;
                                    return (op term)}
                in try opRest <|> termFG
termFG :: Parser (Expr Float)
termFG = (notOp factorFG) <|> factorFG

factorFG :: Parser (Expr Float)
factorFG = try numParseF <|> varParse

numParseF :: Parser (Expr Float)
numParseF = do {i <- float;
            return (Const i)}



{-Helper parsers-}

{-These are merely helper parsers, used to parse letters, numbers, functions, and opertors-}

{-Just parses everystance of a single letter in the string-}
varParse :: Parser (Expr a)
varParse = do { s <- many1 letter;
                return (Var s)}         

{-Just assigns certain strings with operators-}
setOpMain :: Parser (Expr a -> Expr a-> Expr a)
setOpMain = (do { symbol "+"; return Add })
     <|> do { symbol " + "; return Add }
     <|> do { symbol "*"; return Mult } 
     <|> do { symbol " * "; return Mult } 
     <|> do { symbol "^"; return Exponent}
     <|> do { symbol " ^ "; return Exponent}

{-Just assigns certain strings with functions-}
setOpRest :: Parser (Expr a -> Expr a)
setOpRest = (do { string "e"; return Exp })
    <|> do { string "ln"; return Ln } 
    <|> do { string "cos"; return Cos}
    <|> do { string "sin"; return Sin}
        
{-Just parses and negative sign to Neg-}
notOp :: Parser (Expr a) -> Parser (Expr a)
notOp p = do { symbol "-" ;
               exp <- p ;
               return $ Neg exp } 




{-Utility combinators. These are essentially helper parses, which can certain things like symbols, letters, and numbers-}               


{-This utility combinator parses symbols such as; ^,*,+, etc. -}
symbol :: String -> Parser String
symbol ss = let
    symbol' :: Parser String
    symbol' = do { ss' <- string ss;
                    return ss' }
    in try symbol'


{-This parses the instance of a digit for as many times as it occurs. It allows for
something like "1111" to be parsed as 1111 instead of just 1-}
digits :: Parser String
digits = many1 digit

{-This deals with parsing negative digits that begin with a "-" -}
negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                    dig <- digits ;
                    return (neg ++ dig) }

{-Combining negDigits and digits to parse an number as either a positive digit or a negative one-}
integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits

int :: Parser Int
int = fmap read $ try negDigits <|> digits

doubleDigits :: Parser String
doubleDigits = do { ds <- try negDigits <|> digits ;
                    rs <- try decimalDigits <|> return "" ;
                    return $ ds ++ rs }


{-This parses decial digits which are split up by a ".". -}
decimalDigits :: Parser String
decimalDigits = do { d <- char '.' ;
                     rm <- digits ;
                     return $ d:rm }


{-Specifically parses instance of doubles-}
double :: Parser Double
double = fmap read $ doubleDigits

float:: Parser Float
float = fmap read $ doubleDigits
{-site: http://www.cas.mcmaster.ca/~dalvescb/Code/Haskell02SolP.hs-}
{-site: https://github.com/chenc118/CS1XA3/blob/master/Assign3/ExprParser.hs-}