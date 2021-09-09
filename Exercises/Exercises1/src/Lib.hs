module Lib where

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move East  (x,y) = (x+1, y)
move South (x,y) = (x, y-1)

moves :: [Direction] -> Pos -> Pos
moves xs pos = foldl (flip move) pos xs

data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord) 

addNat :: Nat -> Nat -> Nat
addNat x Zero = x
addNat x (Succ y) = Succ (addNat x y)

mulNat :: Nat -> Nat -> Nat
mulNat _ Zero = Zero
mulNat x (Succ y) = addNat (mulNat x y) x

nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (Succ y) = 1 + nat2Int y

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n-1))

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Eq, Show, Read, Ord)

insert :: Ord a => Tree a -> a -> Tree a
insert Leaf x = Node x Leaf Leaf
insert t@(Node a l r) x | x == a = t
                        | x < a = Node a (insert l x) r
                        | x > a = Node a l (insert r x)



morseToLetter :: String -> String
morseToLetter morse = case morse of
     ".-"   -> "A"
     "-..." -> "B"
     "-.-." -> "C"
     "-.."  -> "D"
     "."    -> "E"
     "..-." -> "F"
     "--."  -> "G"
     "...." -> "H"
     ".."   -> "I"
     ".---" -> "J"
     "-.-"  -> "K"
     ".-.." -> "L"
     "--"   -> "M"
     "-."   -> "N"
     "---"  -> "O"
     ".--." -> "P"
     "--.-" -> "Q"
     ".-."  -> "R"
     "..."  -> "S"
     "-"    -> "T"
     "..-"  -> "U"
     "...-" -> "V"
     ".--"  -> "W"
     "-..-" -> "X"
     "-.--" -> "Y"
     "--.." -> "Z"
     _ -> ""

decode :: String -> String
decode [] = []




