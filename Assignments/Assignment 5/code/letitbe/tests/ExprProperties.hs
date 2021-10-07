module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E
import Data.List (intersect, isInfixOf)

instance Arbitrary Expr where
   arbitrary = expr

-- from slides
expr :: Gen Expr
expr = sized exprN

exprN :: Int -> Gen Expr
exprN 0 = fmap Const arbitrary
exprN n = oneof [ fmap Const arbitrary
                 , fmap Var arbitrary
                 , Oper Plus <$> subexpr <*> subexpr
                 , Oper Minus <$> subexpr <*> subexpr
                 , Oper Times <$> subexpr <*> subexpr
                 , Let "x" <$> subexpr `suchThat` (not . isInfixOf "Var" . show) <*> subexpr]
          where subexpr = exprN (n `div`2)

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = E.eval (E.simplify x) mempty === E.eval x mempty

-- generate [char]