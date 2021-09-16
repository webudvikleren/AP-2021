-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\e -> (Right a, []))
  m >>= f = Comp (\e -> case runComp m e of
                        (Left x, ls) -> (Left x, ls)
                        (Right y, ls) -> case runComp (f y) e of
                          (Left z, ls') -> (Left z, ls')
                          (Right v, ls') -> (Right v, ls <> ls'))


-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort re = Comp (\e -> (Left re, []))

look :: VName -> Comp Value
look varName = Comp (\e -> case lookup varName e of
               Nothing -> (Left (EBadVar varName), [])
               Just x -> (Right x, []))

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding x v m = Comp (\e -> let newE = e ++ [(x,v)]
                                in runComp m newE)

output :: String -> Comp ()
output s = Comp (\e -> (Right (), [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy = undefined

operate :: Op -> Value -> Value -> Either String Value
operate = undefined

apply :: FName -> [Value] -> Comp Value
apply = undefined

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval = undefined

exec :: Program -> Comp ()
exec = undefined

execute :: Program -> ([String], Maybe RunError)
execute = undefined
