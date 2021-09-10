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

showExp :: Exp -> String
showExp (Cst x) = show x
showExp (Add e1 e2) = "(" ++ showExp e1 ++ "+" ++ showExp e2 ++ ")"
showExp (Sub e1 e2) = "(" ++ showExp e1 ++ "-" ++ showExp e2 ++ ")"
showExp (Mul e1 e2) = "(" ++ showExp e1 ++ "*" ++ showExp e2 ++ ")"
showExp (Div e1 e2) = "(" ++ showExp e1 ++ "`div`" ++ showExp e2 ++ ")"
showExp (Pow e1 e2) = "(" ++ showExp e1 ++ "^" ++ showExp e2 ++ ")"
showExp _ = error "Expression cannot be handled (yet)."


evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add e1 e2) = evalSimple e1 + evalSimple e2
evalSimple (Sub e1 e2) = evalSimple e1 - evalSimple e2
evalSimple (Mul e1 e2) = evalSimple e1 * evalSimple e2
evalSimple (Div e1 e2) | evalSimple e2 == 0 = error "Division by zero."
                       | otherwise =  div (evalSimple e1) (evalSimple e2)
evalSimple (Pow e1 e2) = evalSimple e1 ^ evalSimple e2
evalSimple _ = error "Expression cannot be handled (yet)"


extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \input -> if input == v then Just n else r input

evalFull :: Exp -> Env -> Integer
evalFull (Cst x) _ = x
evalFull (Add e1 e2) env = evalFull e1 env + evalFull e2 env
evalFull (Sub e1 e2) env = evalFull e1 env - evalFull e2 env
evalFull (Mul e1 e2) env = evalFull e1 env * evalFull e2 env
evalFull (Div e1 e2) env | evalFull e2 env == 0 = error "Division by zero."
                         | otherwise =  div (evalFull e1 env) (evalFull e2 env)
evalFull (Pow e1 e2) env = evalFull e1 env ^ evalFull e2 env
evalFull (If test yes no) env = if evalFull test env /= 0 then 
                                              evalFull yes env else 
                                              evalFull no env
evalFull (Var variableName) env = case env variableName of
                                    Nothing -> error "Variable not bound"
                                    Just a -> a
evalFull (Let var def body) env = evalFull body (extendEnv var (evalFull def env) env)
-- Only error if error in defining expression and using it in body expression - lazy Haskell
evalFull (Sum var from to body) env = if evalFull from env > evalFull to env then 0
                                      else evalFull (Add (Let var from body) (Sum var (Cst (evalFull from env + 1)) to body)) env

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = undefined

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
