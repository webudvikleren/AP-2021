-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit
import Data.Either (isLeft)

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "\nTest suite" [
  testGroup "numConst tests" [
        testCase "1" $ parseString "1" @=? Right [SExp (Const (IntVal 1))],
        testCase "0" $ parseString "0" @=? Right [SExp (Const (IntVal 0))],
        testCase "-0" $ parseString "-0" @=? Right [SExp (Const (IntVal 0))],
        testCase "-1" $ parseString "-1" @=? Right [SExp (Const (IntVal (-1)))],
        testCase "00" $ let x = parseString "00" 
                            x' = isLeft x
                            in x' @=? True,
        testCase "- 1" $  let x = parseString "- 1" 
                              x' = isLeft x
                              in x' @=? True],
  testGroup "ident tests" [
        testCase "foo" $ parseString "foo" @=? Right [SExp (Var "foo")],
        testCase "_foo" $ parseString "_foo" @=? Right [SExp (Var "_foo")],
        testCase "1foo" $ let x = parseString "1foo"
                              x' = isLeft x
                              in x' @=? True,
        testCase "foo1" $ parseString "foo1" @=? Right [SExp (Var "foo1")],
        testCase "True = 5" $ let x = parseString "True = 5"
                                  x' = isLeft x
                                  in x' @=? True],
  testGroup "stringConst tests" [ -- TODO: make more string tests, when it works.
        testCase "AP" $ parseString "'AP'" @=? 
                        Right [SExp (Const (StringVal "AP"))],
        testCase "AP newline" $ parseString "'AP\n'" @=?
                                Right [SExp (Const (StringVal "AP\n"))],
        testCase "AP'" $ let x = parseString "'AP''"
                             x' = isLeft x
                             in x' @=? True,
        testCase "AP backslash" $ let x = parseString "'AP\'"
                                      x' = isLeft x
                                      in x' @=? True,
        testCase "AP 2xbackslash" $ parseString "'AP\\'" @=?
                                    Right [SExp (Const (StringVal "AP\\"))],
        testCase "'fo\\o\
                 \b\na\'r'" $ parseString "'fo\\o\
                  \b\na\'r'" @=? Right [SExp (Const (StringVal "fo\\ob\na'r"))]
        ],
  testGroup "#comments" [
        testCase "foo = 5 #bar" $ parseString "foo = 5 #bar" @=?
                                  Right [SDef "foo" (Const (IntVal 5))],
        testCase "'AP#AAD'" $ parseString "'AP#AAD'" @=? 
                              Right [SExp (Const (StringVal "AP#AAD"))],
        testCase "5;#foo5" $ parseString "5;#foo5" @=?
                      Right [SExp (Const (IntVal 5)),SExp (Const (IntVal 5))]],
  testGroup "Relational operators" [
       testCase "1==1" $ parseString "1==1" @=?
                  Right [SExp (Oper Eq (Const (IntVal 1)) (Const (IntVal 1)))],
       testCase "1<=1" $ parseString "1<=1" @=?
       Right [SExp (Not (Oper Greater (Const (IntVal 1)) (Const (IntVal 1))))],
       testCase "1<1" $ parseString "1<1" @=?
                Right [SExp (Oper Less (Const (IntVal 1)) (Const (IntVal 1)))],
       testCase "1>=1" $ parseString "1>=1" @=?
          Right [SExp (Not (Oper Less (Const (IntVal 1)) (Const (IntVal 1))))],
       testCase "1>1" $ parseString "1>1" @=?
             Right [SExp (Oper Greater (Const (IntVal 1)) (Const (IntVal 1)))],
       testCase "1!=1" $ parseString "1!=1" @=?
            Right [SExp (Not (Oper Eq (Const (IntVal 1)) (Const (IntVal 1))))],
       testCase "1 in [1,2]" $ parseString "1 in [1,2]" @=?
                            Right [SExp (Oper In (Const (IntVal 1))
                                  (List [Const (IntVal 1),Const (IntVal 2)]))],
       testCase "1 not in [1,2]" $ parseString "1 not in [1,2]" @=?
                            Right [SExp (Not (Oper In (Const (IntVal 1))
                                  (List [Const (IntVal 1),Const (IntVal 2)])))],
       testCase "1<2<3" $ let x = parseString "1<2<3"
                              x' = isLeft x
                              
                              in x' @=? True,
       testCase "1<(2<3)" $ parseString "1<(2<3)" @=?
                            Right [SExp (Oper Less (Const (IntVal 1))
                            (Oper Less (Const (IntVal 2)) (Const (IntVal 3))))]  
  ]
      ]
