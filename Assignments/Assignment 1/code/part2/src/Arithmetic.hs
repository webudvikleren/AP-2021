-- This is a skeleton file for you to edit

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

{-
-- summary: forms an expression as a string.
-- params: Exp, e.g. Add (Cst 1) (Cst 1)
-- return value: String, e.g. "(1+1)"
-- remarks: Cannot handle expressions: If, Var, Let and Sum. 
-}
showExp :: Exp -> String
showExp (Cst x) = show x
showExp (Add e1 e2) = showExpSub e1 e2 "+"
showExp (Sub e1 e2) = showExpSub e1 e2 "-"
showExp (Mul e1 e2) = showExpSub e1 e2 "*"
showExp (Div e1 e2) = showExpSub e1 e2 "`div`"
showExp (Pow e1 e2) = showExpSub e1 e2 "^"
showExp _ = error "Expression cannot be handled (yet)."

{-
-- summary: aux function to simplify showExp a bit.
-- params: Exp, Exp, String (operator)
-- return value: String, e.g. the resulting expression formed as a string.
-- remarks: Cannot handle expressions: If, Var, Let and Sum. 
-}
showExpSub :: Exp -> Exp -> String -> String
showExpSub e1 e2 operator = "(" ++ showExp e1 ++ operator ++ showExp e2 ++ ")" 
-- Wraps expression and math operation in paratheses

{-
-- summary: evaluates a simple expression
-- params: Exp, e.g. Add (Cst 1) (Cst 1)
-- return value: Integer, e.g. result 2
-- remarks: Cannot handle expressions: If, Var, Let and Sum. 
-}
evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add e1 e2) = evalSimple e1 + evalSimple e2
evalSimple (Sub e1 e2) = evalSimple e1 - evalSimple e2
evalSimple (Mul e1 e2) = evalSimple e1 * evalSimple e2
evalSimple (Div e1 e2) = if evalSimple e2 == 0 then error "Division by zero."
                         else div (evalSimple e1) (evalSimple e2)
evalSimple (Pow e1 e2) = if evalSimple e2 < 0 then error "Exponent is negative."
                         else evalSimple e1 ^ evalSimple e2
evalSimple _ = error "Expression cannot be handled (yet)"

{-
-- summary: extends a current environment.
-- params: VName :: String: the name of the new variable
           Integer: the number we want to bind to VName
           Env: the current evironment
-- return value: Env, a new extended environment.
-- remarks: Create a basic env with initEnv. 
-}
extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \input -> if input == v then Just n else r input

{-
-- summary: evaluates all the available expressions 
-- params: Exp, Env.
-- return value: Integer, the result of the expression.
-- remarks: 'Let' only errors if there is an error in the defining expression
-- and it is used in the body expression. Unused variables does not cause an
-- error.
-- Errors will cause a runtime error.
-}
evalFull :: Exp -> Env -> Integer
evalFull (Cst x) _ = x
evalFull (Add e1 e2) r = evalFull e1 r + evalFull e2 r
evalFull (Sub e1 e2) r = evalFull e1 r - evalFull e2 r
evalFull (Mul e1 e2) r = evalFull e1 r * evalFull e2 r
evalFull (Div e1 e2) r = if evalFull e2 r == 0 then error "Div by zero."
                         else div (evalFull e1 r) (evalFull e2 r)
evalFull (Pow e1 e2) r = if evalFull e2 r < 0 then error "Neg Exponent."
                         else evalFull e1 r ^ evalFull e2 r
evalFull (If test yes no) r = if evalFull test r /= 0 then 
                                              evalFull yes r else 
                                              evalFull no r
evalFull (Var variableName) r = case r variableName of
                                    Nothing -> error "Variable not bound"
                                    Just a -> a
evalFull (Let var def body) r = evalFull body (extendEnv var (evalFull def r) r)
evalFull (Sum var from to body) r = if evalFull from r > evalFull to r then 0
                                else evalFull (Add (Let var from body)
                                (Sum var (Cst (evalFull from r + 1)) to body)) r
{-
-- summary: evaluates all the available expressions 
-- params: Exp, Env.
-- return value: Either an integer (the result) or an ArithError (error).
-- remarks: 'Let' only errors if there is an error in the defining expression
-- and it is used in the body expression. Unused variables does not cause an
-- error.
-- The function cannot cause a runtime error (unless it is OOM).
-}
evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst x) _ = Right x
evalErr (Add e1 e2) r = do {x <- evalErr e1 r ; y <- evalErr e2 r ;return (x+y)}
evalErr (Sub e1 e2) r = do {x <- evalErr e1 r ; y <- evalErr e2 r ;return (x-y)}
evalErr (Mul e1 e2) r = do {x <- evalErr e1 r ; y <- evalErr e2 r ;return (x*y)}
evalErr (Div e1 e2) r = do {y <- evalErr e1 r ; x <- evalErr e2 r ;
                          if x == 0 then Left EDivZero else return (div y x)}
evalErr (Pow e1 e2) r = do {y <- evalErr e1 r ; x <- evalErr e2 r ;
                          if x < 0 then Left ENegPower else return (y ^ x)}
evalErr (If test yes no) r = do {x <- evalErr test r ;
                               if x /= 0 then evalErr yes r else evalErr no r}
evalErr (Var variableName) r = case r variableName of
                                    Nothing -> Left (EBadVar variableName)
                                    Just a -> Right a
evalErr (Let var def body) r = evalErr body (extendEnv var (evalFull def r) r)
evalErr (Sum var from to body) r = do {
      x <- evalErr from r ;
      y <- evalErr to r ;
      if x > y then Right 0 ;
      else evalErr (Add (Let var from body) (Sum var (Cst (x + 1)) to body)) r}

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
