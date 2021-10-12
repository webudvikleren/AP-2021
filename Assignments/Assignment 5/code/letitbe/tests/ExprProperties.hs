module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E
import Data.List (intersect, isInfixOf)

instance Arbitrary Expr where
   arbitrary = expr


expr :: Gen Expr
expr = sized exprN

-- for generating a small set of variables.
str :: Gen String
str = elements ["x", "y", "z"]

exprN :: Int -> Gen Expr
exprN 0 = fmap Const arbitrary
exprN n = oneof [ fmap Const arbitrary
                 , fmap Var str
                 , Oper Plus <$> subexpr <*> subexpr
                 , Oper Minus <$> subexpr <*> subexpr
                 , Oper Times <$> subexpr `suchThat` (not . isInfixOf "Var" . show) <*>
                                  subexpr `suchThat` (not . isInfixOf "Var" . show)
                 , Let "x" <$> subexpr `suchThat` (not . isInfixOf "Var" . show) <*> subexpr]
          where subexpr = exprN (n `div`2)



prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = E.eval (E.simplify x) mempty === E.eval x mempty

prop_eval_simplify_stats :: Expr -> Property
prop_eval_simplify_stats e = collect e prop_eval_simplify

