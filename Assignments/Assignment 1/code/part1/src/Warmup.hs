module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move East  (x,y) = (x+1, y)
move South (x,y) = (x, y-1)
-- complete the definition

moves :: [Direction] -> Pos -> Pos
moves xs pos = foldl (flip move) pos xs
-- replace with actual definition of moves, and likewise for the
-- other 'undefined' functions

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add x Zero = x
add x (Succ y) = Succ (add x y)

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult x (Succ y) = add (mult x y) x

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ y) = 1 + nat2int y

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x t@(Node a l r)   | x == a = t
                        | x < a = Node a (insert x l) r
                        | x > a = Node a l (insert x l)

-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)

--pinsert :: FIXME  -- uncomment and replace with the proper type of pinsert
pinsert :: Ord a => a -> PTree a -> PTree a
pinsert x PLeaf = PNode x PLeaf PLeaf
pinsert x t@(PNode a l r) | x == a = t
                        | x < a = PNode a (pinsert x l) r
                        | x > a = PNode a l (pinsert x r)
