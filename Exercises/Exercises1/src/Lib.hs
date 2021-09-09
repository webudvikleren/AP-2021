module Lib where
import Data.Char

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move West (x,y) = (x-1, y)
move East (x,y) = (x+1, y)

moves :: [Direction] -> Pos -> Pos
moves [] position = position
moves (h:t) position = moves t (move h position)

data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord)

natAddition :: Nat -> Nat -> Nat
natAddition Zero Zero = Zero
natAddition Zero (Succ x) = (Succ x)
natAddition (Succ x) Zero = (Succ x)
natAddition (Succ x) (Succ y) = Succ (Succ ( natAddition x y))

natMultiplication :: Nat -> Nat -> Nat
natMultiplication _ Zero = Zero
natMultiplication Zero _ = Zero
natMultiplication (Succ x) y = natAddition (natMultiplication x y) y

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = 1 + nat2int(x)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ(int2nat(x - 1))

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Eq, Show, Read, Ord)

insert :: Ord a => Tree a -> a -> Tree a
insert Leaf x = Node x Leaf Leaf
insert (Node x left right) y
    | y < x = Node x (insert left y) right
    | y > x = Node x left (insert right y)
    | otherwise = (Node x left right)

letterToMorse :: Char -> String
letterToMorse letter = case letter of
    'a' -> ".-"
    'b' -> "-..."
    'c' -> "-.-."
    'd' -> "-.."
    'e' -> "."
    'f' -> "..-."
    'g' -> "--."
    'h' -> "...."
    'i' -> ".."
    'j' -> ".---"
    'k' -> "-.-"
    'l' -> ".-.."
    'm' -> "--"
    'n' -> "-."
    'o' -> "---"
    'p' -> ".--."
    'q' -> "--.-"
    'r' -> ".-."
    's' -> "..."
    't' -> "-"
    'u' -> "..-"
    'v' -> "...-"
    'w' -> ".--"
    'x' -> "-..-"
    'y' -> "-.--"
    'z' -> "--.."
    _ -> error "Letter didn't match"

morseToLetter :: String -> Char
morseToLetter morse = case morse of
    ".-" -> 'a'
    "-..." -> 'b'
    "-.-." -> 'c'
    "-.." -> 'd'
    "." -> 'e'
    "..-." -> 'f'
    "--." -> 'g'
    "...." -> 'h'
    ".." -> 'i'
    ".---" -> 'j'
    "-.-" -> 'k'
    ".-.." -> 'l'
    "--" -> 'm'
    "-." -> 'n'
    "---" -> 'o'
    ".--." -> 'p'
    "--.-" -> 'q'
    ".-." -> 'r'
    "..." -> 's'
    "-" -> 't'
    "..-" -> 'u'
    "...-" -> 'v'
    ".--" -> 'w'
    "-..-" -> 'x'
    "-.--" -> 'y'
    "--.." -> 'z'
    _ -> error "Morse didn't match"

encode :: String -> String
encode x = concatMap letterToMorse [ toLower c | c <- x]

slice :: String -> Int -> Int -> String
slice xs from to = take (to - from + 1) (drop from xs)

decode :: String -> Int -> Int -> [String]
decode "" _ _ = [""]
decode str from to
    | (to >= from) && (to < (length str)) = [(slice str from to)] ++ (decode str (from + 1) to) ++ (decode str from (to + 1))
    | otherwise = []