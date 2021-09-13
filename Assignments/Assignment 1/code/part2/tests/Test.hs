-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

tests :: [(String, Bool)]
tests = [
  test0,
  test1,
  test2,
  test3,
  test4,
  test5,
  test6,
  test7,
  test8,
  test9,
  test10,
  test11,
  test12,
  test13,
  test14,
  test15,
  test16,
  test17,
  test18,
  test19,
  test20,
  test21,
  test22,
  test23,
  test24,
  test25,
  test26,
  test27,
  test28,
  test29,
  test30,
  test31,
  test32,
  test33,
  test34,
  test35,
  test36,
  test37,
  test38,
  test39,
  test40,
  test41] where
  --Show expressions
  test0 = ("test0", showExp (Cst 2) == "2")
  test1 = ("test1", showExp (Add (Cst 2) (Cst 4)) == "(2+4)")
  test2 = ("test2", showExp (Sub (Cst 2) (Cst 4)) == "(2-4)")
  test3 = ("test3", showExp (Mul (Cst 2) (Cst 4)) == "(2*4)")
  test4 = ("test4", showExp (Div (Cst 2) (Cst 4)) == "(2`div`4)")
  test5 = ("test5", showExp (Pow (Cst 2) (Cst 4)) == "(2^4)")
  test6 = ("test6", showExp (Pow (Cst 2) (Add (Cst 5) (Cst 10))) == "(2^(5+10))")
  test7 = ("test7", showExp (Pow (Add (Cst 5) (Cst 10)) (Cst 2)) == "((5+10)^2)")
  --Simple expressions
  test8 = ("test8", evalSimple (Add (Cst 2) (Cst 4)) == 6)
  test9 = ("test9", evalSimple (Sub (Cst 4) (Cst 2)) == 2)
  test10 = ("test10", evalSimple (Mul (Cst 4) (Cst 2)) == 8)
  test11 = ("test11", evalSimple (Div (Cst 6) (Cst 2)) == 3)
  test12 = ("test12", evalSimple (Pow (Cst 2) (Cst 4)) == 16)
  --Full expressions
  test13 = ("test13", evalFull (Add (Cst 2) (Cst 4)) initEnv == 6)
  test14 = ("test14", evalFull (Sub (Cst 4) (Cst 2)) initEnv == 2)
  test15 = ("test15", evalFull (Mul (Cst 4) (Cst 2)) initEnv == 8)
  test16 = ("test16", evalFull (Div (Cst 6) (Cst 2)) initEnv == 3)
  test17 = ("test17", evalFull (Pow (Cst 2) (Cst 4)) initEnv == 16)
  test18 = ("test18", evalFull (Div (Add (Cst 6) (Cst 8)) (Sub (Cst 10) (Cst 3))) initEnv == 2)
  test19 = ("test19", evalFull (If (Sub (Cst 5) (Cst 5)) (Cst 6) (Cst 7)) initEnv == 7)
  test20 = ("test20", evalFull (If (Sub (Cst 5) (Cst 4)) (Cst 6) (Cst 7)) initEnv == 6)
  test21 = ("test21", evalFull (Var "x") (extendEnv "x" 4 initEnv) == 4)
  test22 = ("test22", evalFull (Let "x" (Div (Cst 20) (Cst 5)) (Add (Cst 3) (Sub (Cst 5) (Var "x")))) initEnv == 4)
  test23 = ("test23", evalFull (Sum "x" (Cst 1) (Add (Cst 2) (Cst 2)) (Mul (Var "x") (Var "x"))) initEnv == 30)
  test24 = ("test24", evalFull (Sum "x" (Cst 1) (Mul (Cst 5) (Cst 5)) (Var "x")) initEnv == 325)
  --Error expressions corrects
  test25 = ("test25", evalErr (Add (Cst 2) (Cst 4)) initEnv == Right 6)
  test26 = ("test26", evalErr (Sub (Cst 4) (Cst 2)) initEnv == Right 2)
  test27 = ("test27", evalErr (Mul (Cst 4) (Cst 2)) initEnv == Right 8)
  test28 = ("test28", evalErr (Div (Cst 6) (Cst 2)) initEnv == Right 3)
  test29 = ("test29", evalErr (Pow (Cst 2) (Cst 4)) initEnv == Right 16)
  test30 = ("test30", evalErr (Div (Add (Cst 6) (Cst 8)) (Sub (Cst 10) (Cst 3))) initEnv == Right 2)
  test31 = ("test31", evalErr (If (Sub (Cst 5) (Cst 5)) (Cst 6) (Cst 7)) initEnv == Right 7)
  test32 = ("test32", evalErr (If (Sub (Cst 5) (Cst 4)) (Cst 6) (Cst 7)) initEnv == Right 6)
  test33 = ("test33", evalErr (Var "x") (extendEnv "x" 4 initEnv) == Right 4)
  test34 = ("test34", evalErr (Let "x" (Div (Cst 20) (Cst 5)) (Add (Cst 3) (Sub (Cst 5) (Var "x")))) initEnv == Right 4)
  test35 = ("test35", evalErr (Sum "x" (Cst 1) (Add (Cst 2) (Cst 2)) (Mul (Var "x") (Var "x"))) initEnv == Right 30)
  test36 = ("test36", evalErr (Sum "x" (Cst 1) (Mul (Cst 5) (Cst 5)) (Var "x")) initEnv == Right 325)
  --Error expressions errors
  test37 = ("test37", evalErr (Var "y") (extendEnv "x" 4 initEnv) == Left (EBadVar "y"))
  test38 = ("test38", evalErr (Div (Cst 6) (Cst 0)) initEnv == Left EDivZero)
  --Error expressions testing left-first-evaluation
  test39 = ("test39", evalErr (Pow (Cst 2) (Sub (Cst 4) (Cst 7))) initEnv == Left ENegPower)
  test40 = ("test40", evalErr (Pow (Div (Cst 2) (Cst 0)) (Sub (Cst 4) (Cst 7))) initEnv == Left EDivZero)
  test41 = ("test41", evalErr (Pow (Div (Var "y") (Cst 0)) (Sub (Cst 4) (Cst 7))) (extendEnv "x" 4 initEnv) == Left (EBadVar "y"))

main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
