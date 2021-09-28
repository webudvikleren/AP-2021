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
  testGroup "stringConst tests" [ 
        testCase "AP" $ parseString "'AP'" @=? 
                        Right [SExp (Const (StringVal "AP"))],
        testCase "AP newline" $ parseString "'AP\\n'" @=?
                                Right [SExp (Const (StringVal "AP\n"))],
        testCase "AP'" $ let x = parseString "'AP''"
                             x' = isLeft x
                             in x' @=? True,
        testCase "AP backslash'" $ let x = parseString "'AP\''"
                                       x' = isLeft x
                                       in x' @=? True,
        testCase "'fo\\no'"  $ parseString "'fo\\no'" @=?
                                   Right [SExp (Const (StringVal "fo\no"))],
        testCase "'\\n '" $ parseString "'\\n '" @=?
                            Right [SExp (Const (StringVal "\n "))],
        testCase "'\\ '" $ let x = parseString "'\\ '"
                               x' = isLeft x
                               in x' @=? True,
        testCase " 'AP' " $ parseString " 'AP' " @=?
                           Right [SExp (Const (StringVal "AP"))]],
  testGroup "#comments" [
        testCase "foo = 5 #bar" $ parseString "foo = 5 #bar" @=?
                                  Right [SDef "foo" (Const (IntVal 5))],
        testCase "'AP#AAD'" $ parseString "'AP#AAD'" @=? 
                              Right [SExp (Const (StringVal "AP#AAD"))],
        testCase "#NL1 #aNL" $ parseString "#\n1 #a\n" @=?
                              Right [SExp (Const (IntVal 1))],
        testCase "#NL5 #NL#NL" $ parseString "#\n5 #\n#\n" @=?
                              Right [SExp (Const (IntVal 5))],
        testCase "#NL5;#NL6#NL" $ parseString "#\n5;#\n6#\n" @=?
            Right [SExp (Const (IntVal 5)),SExp (Const (IntVal 6))]],
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
                          (Oper Less (Const (IntVal 2)) (Const (IntVal 3))))]],
  testGroup "Arithmetic operators" [
       testCase "1+" $ let x = parseString "1+"
                           x' = isLeft x
                           in x' @=? True,
       testCase "+1" $ let x = parseString "+1"
                           x' = isLeft x
                           in x' @=? True,
       testCase "1+1" $ parseString "1+1" @=? 
                Right [SExp (Oper Plus (Const (IntVal 1)) (Const (IntVal 1)))],
       testCase "-1+1" $ parseString "-1+1" @=? 
             Right [SExp (Oper Plus (Const (IntVal (-1))) (Const (IntVal 1)))],
       testCase "1-1" $  parseString "1-1" @=? 
               Right [SExp (Oper Minus (Const (IntVal 1)) (Const (IntVal 1)))],
       testCase "1*1" $  parseString "1*1" @=? 
               Right [SExp (Oper Times (Const (IntVal 1)) (Const (IntVal 1)))],
       testCase "1//1" $  parseString "1//1" @=? 
                 Right [SExp (Oper Div (Const (IntVal 1)) (Const (IntVal 1)))],
       testCase "1%1" $  parseString "1%1" @=? 
                 Right [SExp (Oper Mod (Const (IntVal 1)) (Const (IntVal 1)))]],
  testGroup "Terminals" [
       testCase "None" $ parseString "None" @=? Right [SExp (Const NoneVal)],
       testCase "False" $ parseString "False" @=? Right [SExp (Const FalseVal)],
       testCase "True" $ parseString "True" @=? Right [SExp (Const TrueVal)],
       testCase "Foo = 5" $ parseString "Foo = 5" @=?
                                        Right [SDef "Foo" (Const (IntVal 5))],
       testCase "range(1,2)" $ parseString "range (1,2)" @=?
       Right [SExp (Call "range" [Const (IntVal 1), Const (IntVal 2)])],
       testCase "[1,2]" $ parseString "[1,2]" @=? 
                       Right [SExp (List [Const (IntVal 1), Const (IntVal 2)])],
       testCase "[1,2" $ let x = parseString "[1,2"
                             x' = isLeft x
                             in x' @=? True,
       testCase "[x for x in range(1,2)]" $ parseString "[x for x in range(1,2)]" @=?
       Right [SExp (Compr (Var "x") [CCFor "x" (Call "range" [Const (IntVal 1),
                                                          Const (IntVal 2)])])],
       testCase "[x for x in [] if x > 0]" $ parseString "[x for x in [] if x > 0]" @=?
       Right [SExp (Compr (Var "x") [CCFor "x" (List []),CCIf (Oper Greater (Var "x")
                                                         (Const (IntVal 0)))])],
       testCase "[if x > 0]" $ let x = parseString "[if x > 0]"
                                   x' = isLeft x
                                   in x' @=? True,
       testCase "(1+1)" $ parseString "(1+1)" @=? 
           Right [SExp (Oper Plus (Const (IntVal 1)) (Const (IntVal 1)))]],
  testGroup "Expressions" [
       testCase "not (1+1)" $ parseString "not (1+1)" @=?
         Right [SExp (Not (Oper Plus (Const (IntVal 1)) (Const (IntVal 1))))],
       testCase "not (1+1" $ let x = parseString "not (1+1"
                                 x' = isLeft x
                                 in x' @=? True,
       testCase "None + True" $ parseString "None + True" @=?
         Right [SExp (Oper Plus (Const NoneVal) (Const TrueVal))]
  ],
  testGroup "Statements/Program" [
       testCase "AP = 12" $ parseString "AP = 12" @=? 
           Right [SDef "AP" (Const (IntVal 12))],
       testCase "42" $ parseString "42" @=?
           Right [SExp (Const (IntVal 42))],
       testCase "AP = 12; 42" $ parseString "AP = 12; 42" @=?
           Right [SDef "AP" (Const (IntVal 12)), SExp (Const (IntVal 42))],
       testCase "Empty program" $ let x = parseString ""
                                      x' = isLeft x
                                      in x' @=? True]
  
      ]
