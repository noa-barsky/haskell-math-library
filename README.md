# Haskell Math Library


## Overview
The following is a math library made in Haskell for my CS1XA3 class at McMaster University which is intended to help with calculus homework. Full documentation [here](https://noa-barsky.github.io/haskell-math-library/) 
#### As such it can do the following:
1. Perform partial (or full differentiation)
2. Evaluate expressions
3. Simplify expressions to close to normal form (not 100% of the time)
4. Do basic anti-derivatives
5. Perform multi-variant chain rule on an expression with 2 independent variables, and those 2 independent variables with respect to the same single dependent variable
6. Can parse certain strings in a defined data type, `Expr a`
7. Can look up common trig values
## Functionality
```
eval :: Map.Map String a -> Expr a -> a --Evaluates expressions down to one value
simplify :: Map.Map String a -> Expr a -> Expr a --Simplifies things close to normal form
partDiff :: String -> Expr a -> Expr a - - partially or fully evaluates expressions
multDiff :: Expr a -> Expr a -> Expr a -> String -> String -> String -> Expr a - - performs the multi-variant chain rule
antiDeriv :: Expr a -> Expr a - - performs basic anti-derivatives
```

## To do
Unfortunately, while doing this project I ran out of time and I did not get to complete all the features I wanted. I decided this will be version 1 and in upcoming versions I would want to add:
1. Fix my parsers. I would want my parsers to be able to parse parenthesis and as such respect order of operations. As well, I would like to be able to parse expressions like “cos x + 4” as of right now, I cannot combine my functions with operators.
2. Add more test cases. Although, I have a lot there is a lot of code and I feel could have been tested
3. Add more equations which are generally annoying to do by hand, but come up frequently in calculus

## Citation:
1. For the parser I followed Allen Chen’s code, see his work [here]( https://github.com/chenc118)
2. For simplifiable I used Leon Yun’s code, see his work [here]( https://github.com/yunc5)
3. The outline of the modules some parts of the parser work I used Curtis D’alves course material, see that [here](http://www.cas.mcmaster.ca/~dalvescb/#outline-container-org2a5d6f3)

---
Documentation and Library by *Noa Barsky*

