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

--- HELPER FUNCTIONS FOR RANGE

-- For checking if list contains other values than IntVals, this is used in the
-- range function that only accepts a list of IntVals.
checkIntVals :: Value -> Bool
checkIntVals (IntVal _) = True
checkIntVals _ = False

-- Creates an Intval from an Int, e.g. 5 -> IntVal 5, this is used to convert
-- back to IntVals when creating lists.
toIntVal :: Int -> Value
toIntVal = IntVal

-- Creates a list given a range and a stepsize. Used because stepsize can be
-- smaller then starting value, e.g. [10,1..100] which is not possible in 
-- the built-in range functionality. So we made our own.
range :: (Ord a, Num a) => a -> a -> a -> [a]
range start end step = takeWhile (<=end) $ iterate (+step) start

-- this is used by the range function in Boa to create lists. Either from 1, 2
-- or 3 arguments. 
makeIntValList :: [Value] -> [Value]
makeIntValList [IntVal x] = map toIntVal [0..x-1]
makeIntValList [IntVal x, IntVal y] = map toIntVal [x..y-1]
makeIntValList [IntVal x, IntVal y, IntVal z] | (x >= y) && (z > 0) = []
                                              | (x <= y) && (z < 0) = []
                                              | otherwise = 
                                                      map toIntVal (range x y z)
makeIntValList _ = undefined 

---HELPER FUNCTIONS FOR PRINT

format :: Bool -> String
format inList = if inList then ", " else " "

--- Dont know how to make this work with ListVal.
valToString :: Bool -> Value -> String
valToString inList NoneVal = "None" ++ format inList
valToString inList TrueVal = "True" ++ format inList
valToString inList FalseVal = "False" ++ format inList
valToString inList (IntVal x) = show x ++ format inList
valToString inList (StringVal s) = s ++ format inList
valToString inList (ListVal []) = "[]"
valToString inList (ListVal (x:xs)) = valToString True x ++
                                      valToString True (ListVal xs)
                                      

--TODO: finish print.
apply :: FName -> [Value] -> Comp Value
apply "range" xs 
      | not (all checkIntVals xs) = abort (EBadArg "Non-integer args.")
      | length xs < 1 || length xs > 3 = abort (EBadArg "Wrong # of args")
      | length xs == 3 && xs !! 2 == IntVal 0 = abort (EBadArg "Stepsize 0")
      | otherwise = return (ListVal (makeIntValList xs))

apply "print" (x:xs) = undefined
apply _ _ = abort (EBadFun "Unknown function.")

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval = undefined

exec :: Program -> Comp ()
exec = undefined

execute :: Program -> ([String], Maybe RunError)
execute = undefined
