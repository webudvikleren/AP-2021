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
extendEnv = undefined

evalFull :: Exp -> Env -> Integer
evalFull = undefined

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr = undefined

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
