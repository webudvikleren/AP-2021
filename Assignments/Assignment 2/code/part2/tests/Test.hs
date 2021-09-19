-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit
import Data.Either

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Test Suite" [
   testGroup "Monad operation tests"
   [testCase "Abort" $ let (r, s) = runComp (abort (EBadVar "test")) []
                           (r', s') = (isLeft r, s)
                           in (r', s) @=? (True, []),
    testCase "look exists" $ let (r, s) = runComp (look "x") [("x", IntVal 1)]
                                 (r', s') = (isRight r, s)
                                 in (r', s') @=? (True, []),
    testCase "look !exists" $ let (r, s) = runComp (look "x") [("y", IntVal 1)]
                                  (r', s') = (isLeft r, s)
                                  in (r', s') @=? (True, []),
    testCase "withBinding" $ let (r, s) = runComp (withBinding "y" (IntVal 2) (look "y")) [("x", IntVal 1)]
                                 (r', s') = (isRight r, s)
                                 in (r', s') @=? (True, []),
    testCase "output" $ let (r, s) = runComp (output "AP") []
                        in (r, s) @=? (Right (), ["AP"])],
   testGroup "Truthy tests"
   [testCase "NoneVal" $ truthy NoneVal @=? False,
    testCase "TrueVal" $ truthy TrueVal @=? True,
    testCase "FalseVal" $ truthy FalseVal @=? False,
    testCase "InVal 0" $ truthy (IntVal 0) @=? False,
    testCase "IntVal 1" $ truthy (IntVal 1) @=? True,
    testCase "IntVal -1" $ truthy (IntVal (-1)) @=? True,
    testCase "StringVal empty" $ truthy (StringVal []) @=? False,
    testCase "StringVal 'AP'" $ truthy (StringVal "AP") @=? True,
    testCase "ListVal empty" $ truthy (ListVal []) @=? False,
    testCase "ListVal [IntVal 1]" $ truthy (ListVal [IntVal 1]) @=? True],
   testGroup "Operate tests"
   [testCase "Plus (1+1)" $ operate Plus (IntVal 1) (IntVal 1) @=? Right (IntVal 2),
    testCase "Plus (1+None)" $ assertEqual [] True (isLeft (operate Plus (IntVal 1) NoneVal)),
    testCase "Minus (1-1)" $ operate Minus (IntVal 1) (IntVal 1) @=? Right (IntVal 0),
    testCase "Minus (1+None)" $ assertEqual [] True (isLeft (operate Minus (IntVal 1) NoneVal)),
    testCase "Times (1+1)" $ operate Times (IntVal 1) (IntVal 1) @=? Right (IntVal 1),
    testCase "Times 1+None" $ assertEqual [] True (isLeft (operate Times (IntVal 1) NoneVal)),
    testCase "Div (1/1)" $ operate Div (IntVal 1) (IntVal 1) @=? Right (IntVal 1),
    testCase "Div (1/None" $ assertEqual [] True (isLeft (operate Div (IntVal 1) NoneVal)),
    testCase "Div (1/0)" $ assertEqual [] True (isLeft (operate Div (IntVal 1) (IntVal 0))),
    testCase "Mod (1/1)" $ operate Mod (IntVal 1) (IntVal 1) @=? Right (IntVal 0),
    testCase "Div (1/None" $ assertEqual [] True (isLeft (operate Mod (IntVal 1) NoneVal)),
    testCase "Div (1/0)" $ assertEqual [] True (isLeft (operate Mod (IntVal 1) (IntVal 0))),
    testCase "Eq 1==1" $ operate Eq (IntVal 1) (IntVal 1) @=? Right TrueVal,
    testCase "Eq 1=='AP'" $ operate Eq (IntVal 1) (StringVal "AP") @=? Right FalseVal,
    testCase "Less 1<2" $ operate Less (IntVal 1) (IntVal 2) @=? Right TrueVal,
    testCase "Less 2<1" $ operate Less (IntVal 2) (IntVal 1) @=? Right FalseVal,
    testCase "Less 1<None" $ assertEqual [] True (isLeft (operate Less (IntVal 1) NoneVal)),
    testCase "Less 1>2" $ operate Greater (IntVal 1) (IntVal 2) @=? Right FalseVal,
    testCase "Less 2>1" $ operate Greater (IntVal 2) (IntVal 1) @=? Right TrueVal,
    testCase "Less 1>None" $ assertEqual [] True (isLeft (operate Greater (IntVal 1) NoneVal)),
    testCase "In 1 'elem' [1,2]" $ operate In (IntVal 1) (ListVal [IntVal 1, IntVal 2]) @=? Right TrueVal,
    testCase "In 1 'elem' [2,3]" $ operate In (IntVal 1) (ListVal [IntVal 2, IntVal 3]) @=? Right FalseVal,
    testCase "In 1 'elem' 2" $ assertEqual [] True (isLeft (operate In (IntVal 1) (IntVal 2)))],
   testGroup "Apply tests"
   [testCase "range [2]" $  runComp (apply "range" [IntVal 2]) [] @=? (Right (ListVal [IntVal 0, IntVal 1]), []),
    testCase "range [2,4]" $ runComp (apply "range" [IntVal 2, IntVal 4]) [] @=? (Right (ListVal [IntVal 2, IntVal 3]), []),
    testCase "range [2,6,2]" $ runComp (apply "range" [IntVal 2, IntVal 6, IntVal 2]) [] @=? (Right (ListVal [IntVal 2, IntVal 4]), []),
    testCase "range []" $ let (x, s) = (runComp (apply "range" []) [])
                              (x', s') = (isLeft x, s)
                              in (x', s) @=? (True, []),
    testCase "range [1,2,3,4]" $ let (x, s) = (runComp (apply "range" [IntVal 1, IntVal 2, IntVal 3, IntVal 4]) [])
                                     (x', s') = (isLeft x, s)
                                     in (x', s) @=? (True, []),
    testCase "range [NoneVal]" $ let (x, s) = (runComp (apply "range" [NoneVal]) [])
                                     (x', s') = (isLeft x, s)
                                     in (x', s) @=? (True, []),
    testCase "range [1,2,0]" $ let (x, s) = (runComp (apply "range" [IntVal 1, IntVal 2, IntVal 0]) [])
                                   (x', s') = (isLeft x, s)
                                   in (x', s) @=? (True, []),
    testCase "print" $ let (x, s) = (runComp (apply "print" [IntVal 42, StringVal "foo", ListVal [TrueVal, ListVal []], IntVal (-1)]) [])
                                        in (x, s) @=? (Right NoneVal, ["42 ", "foo ", "[True, []]", "-1", "\n"]),
    testCase "wrong fun" $ let (x, s) = (runComp (apply "wrong" []) [])
                               (x', s') = (isLeft x, s)
                               in (x', s) @=? (True, [])]]
    
