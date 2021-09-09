module TicTacToe where

data Player = Cross | Nought
            deriving Show
                     
data Cell = Move Player | Empty
          deriving Show

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Move Cross) (Move Cross) = True
  (==) (Move Nought) (Move Nought) = True
  (==) _ _ = False

type Row = [Cell]
type Board = [Row]

emptyBoard :: Board
emptyBoard = take 3 emptyRows
  where emptyRows = repeat emptyRow
        emptyRow = replicate 3 Empty

type Position = (Int, Int)

-- You should probably take a good look at this function until you
-- understand it. Keep in mind that it does not check whether the move
-- is valid.
move :: Position -> Board -> Player -> Board
move (x, y) board player = rowsBefore ++ (changed : rowsAfter)
  where (rowsBefore, toBeChanged : rowsAfter) = splitAt x board
        changed = cellsBefore ++ (newCell : cellsAfter)
        (cellsBefore, old : cellsAfter) = splitAt y toBeChanged
        newCell = Move player
        
-- Type for representing the state of a game. That is, which should
-- take the next move, and what does the board look like.
type GameState = (Player, Board)

startState :: GameState
startState = (Cross, emptyBoard)

-- Hint: You already have the move function, defined above, to do most
-- of the legwork.
makeMove :: Position -> GameState -> GameState
makeMove pos (Cross, b) = (Nought, move pos b Cross)
makeMove pos (Nought, b) = (Cross, move pos b Nought)

validMove :: Position -> GameState -> Bool
validMove (x, y) (p, b) = (b !! x) !! y == Empty && elem (x, y) allMoves

allMoves :: [Position]
allMoves = [ (x, y) | x <- [0 .. 2], y <- [0 .. 2] ]

allValidMoves :: GameState -> [Position]
allValidMoves gs = filter (flip validMove $ gs) allMoves

-- The type for representing game trees.  Each node have a game state
-- and a list of *valid* moves leading to (sub-) game trees
data GameTree = Node GameState [(Position, GameTree)]

-- Grow the game tree starting in a given state
makeTree :: GameState -> GameTree
makeTree gs = Node gs [(pos, gt) | pos <- allValidMoves gs, gt <- [makeTree (makeMove pos gs)]]

-- Return all game states in a game tree, duplicates are allowed
allNodes :: GameTree -> [GameState]
allNodes (Node gs subs) = gs : concatMap (allNodes . snd) subs

-- For reading a board from a string, i.e. readBoard "X  \nX   \nX  "
readBoard :: String -> Board
readBoard = map readRow . lines
  where readRow = map readSquare

readSquare :: Char -> Cell
readSquare ' ' = Empty
readSquare c   = readPiece c

readPiece :: Char -> Cell
readPiece 'X' = Move Cross
readPiece 'O' = Move Nought

-- For showing a board 
showBoard :: Board -> String
showBoard = unlines . map showRow
  where showRow = map showSquare

showSquare :: Cell -> Char
showSquare Empty = ' '
showSquare s = showPiece s

showPiece :: Cell -> Char
showPiece (Move Cross) = 'X'
showPiece (Move Nought) = 'O'

--- minimax?
