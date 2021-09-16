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
truthy NoneVal = False
truthy TrueVal = True
truthy FalseVal = False
truthy (IntVal x) = x /= 0
truthy (StringVal s) = s /= []
truthy (ListVal xs) = xs /= []

operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal x) (IntVal y) = Right (IntVal (x+y))
operate Plus _ _ = Left "Cannot add non-integer values."
operate Minus (IntVal x) (IntVal y) = Right (IntVal (x-y))
operate Minus _ _ = Left "Cannot subtract non-integer values."
operate Times (IntVal x) (IntVal y) = Right (IntVal (x*y))
operate Times _ _ = Left "Cannot multiply non-integer values."
operate Div (IntVal x) (IntVal y) = if y == 0 then Left "Division by zero."
                                    else Right (IntVal (div x y))
operate Div _ _ = Left "Cannot divide non-integer values."
operate Mod (IntVal x) (IntVal y) = if y == 0 then Left "Modulo by zero"
                                    else Right (IntVal (mod x y))
operate Mod _ _ = Left "Cannot modulo non-integer values."
operate Eq x y = if x == y then Right TrueVal else Right FalseVal
operate Less (IntVal x) (IntVal y) = if x < y then Right TrueVal
                                     else Right FalseVal
operate Less _ _ = Left "Cannot compare (<) non-integer values."
operate Greater (IntVal x) (IntVal y) = if x > y then Right TrueVal
                                        else Right FalseVal
operate Greater _ _ = Left "Cannot compare (>) non-integer values."
operate In x (ListVal xs) = if x `elem` xs then Right TrueVal
                            else Right FalseVal
operate In _ _ = Left "Cannot check membership of non-list value."

apply :: FName -> [Value] -> Comp Value
apply = undefined

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval = undefined

exec :: Program -> Comp ()
exec = undefined

execute :: Program -> ([String], Maybe RunError)
execute = undefined
