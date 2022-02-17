{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Dat where
import Brick hiding (Down, Up)
import Data.Matrix
import Prelude hiding (Right, Left)
import System.IO

{- The value of a given cell in a sudoku-grid
Lock x represents a number in the grid that is predefined, and thus immutable,
Input x represents a number in the grid that was input by the player.
Empty is an empty cell
    INVARIANT:  0 < x <= 10 where x == (Input x) || (Lock x)
-}

data Cell = Lock Int    |
            Input Int   |
            Empty       deriving (Eq, Show) --Cell has a locked value (predefined), input value (input by user) or is empty

type Grid       = Matrix Cell   --Matrix of cell values

type Coord      = (Int, Int)    --(Row index, Column index)

{- Direction type to be used for navigating the grid

-}
data Direction  = Up | Down | Left | Right  deriving Show

--The game state passed to defaultMain 
data Game = Game {
    grid        :: Grid,        --The sudoku grid
    focusedCell :: Coord,       --Cell currently in focus of cursor
    complete    :: Bool         --Puzzle complete flag
} deriving Show

type Name = ()
newSudokuMatrix = matrix 9 9 (\(r, c) -> Empty)


--GRID OPERATIONS
{- isLocked grid coord 
Checks if a cell in a grid is Locked
    RETURNS:    True iff the cell at argument coords is Lock
-}


isLocked :: Grid -> Coord -> Bool
isLocked g (r, c) = let cell = getElem r c g in
    case cell of 
        (Lock _)  -> True
        Input _ -> False
        Empty   -> False

{- insert (input i) (r, c) grid
Inserts i into grid at row number r and column number c if the value is within the given boundary.
    RETURNS: the updated version of grid
    EXAMPLES: -
-}
insert :: Cell -> Coord -> Game -> Game
insert (Input i) (r, c) game 
    | isLocked (grid game) (r, c)   = game
    | otherwise                     = Game {grid = setElem (Input i) (r, c) (grid game)}


{- delete (r, c) grid
Deletes a value from position (r, c) in grid
    RETURNS: the updated version of grid
    EXAMPLES: -
-}
delete ::  Coord -> Grid -> Grid
delete (r, c) grid
    | 1 <= r && r <= 9 && 1 <= c && c <= 9 = setElem Empty (r, c) grid
    | otherwise = grid

{- legalInSubGrid (Input i) lst grid
Checks if i exists inside grid's subgrid lst
    RETURNS: True if i doesn't exist in the subgrid lst, False if i does exist in the subgrid lst.
    VARIANT: length lst
    EXAMPLES: -
-}
legalInSubGrid :: Cell -> [Coord] -> Grid -> Bool
legalInSubGrid _ [] _ = True
legalInSubGrid Empty _ _ = True
legalInSubGrid (Input i) lst@(x:xs) grid
    | Input i == uncurry getElem x grid = False
    | otherwise                         = legalInSubGrid (Input i) xs grid

{- listSubGrid (r, c)
Creates a list of every coordinate that exists in the same 3x3 sub-grid as (r, c)
    PRE: 0 < r <= 9, 0 < c <= 9
    RETURNS: a list of every coordinate that exists in the same 3x3 sub-grid as (r, c)
    EXAMPLES: listSubGrid (1, 5) = [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
              listSubGrid (7, 9) = [(7,7),(7,8),(7,9),(8,7),(8,8),(8,9),(9,7),(9,8),(9,9)]
-}
listSubGrid :: Coord -> [Coord]
listSubGrid (r, c) = [(x, y) | x <- coordList r, y <- coordList c] --Inspiration from StackOverflow (https://stackoverflow.com/questions/32093912/all-combinations-of-elements-of-two-lists-in-haskell)

{- coordList x
Creates a list from: 1-3 if x ∈ [1..3], 4-6 if x ∈ [4..6], 7-9 if x ∈ [7..9]
    RETURNS:    a list from: [1..3], [4..6] or [7..9]
    EXAMPLES:   coordList 3 = [1,2,3]
                coordList 2 = [1,2,3]
                coordList 8 = [7,8,9]
-}
coordList :: Int -> [Int]
coordList x
    | 0 < x && x <= 3 = [1..3]
    | 3 < x && x <= 6 = [4..6]
    | 6 < x && x <= 9 = [7..9]
    | otherwise = []

{- legalInRow (Input i) (r, c) grid
Checks if i exists on the row r.
    RETURNS: True if i doesn't exist on r, False if i does exist on r.
    EXAMPLES: -
-}
legalInRow :: Cell -> Coord -> Grid -> Bool
legalInRow Empty _ _ = True
legalInRow (Input i) (r, c) grid = checkRow (Input i) r 1 grid

{- checkRow (Input i) x acc grid
Checks if (Input i) is equal to any of the cells on the row x.
    RETURNS: True if (Input i) isn't equal to any cell on the row x, False if (Input i) is equal to any cell on the row x.
    VARIANT: acc
    EXAMPLES: -
-}
checkRow :: Cell -> Int -> Int -> Grid -> Bool
checkRow (Input i) x acc grid
    | 9 < acc = True
    | getElem x acc grid == Empty = checkRow (Input i) x (acc + 1) grid
    | (Input i) == getElem x acc grid = False
    | otherwise = checkRow (Input i) x (acc + 1) grid

{- legalInCol (Input i) (r, c) grid
Checks if i exists on the column c.
    RETURNS: True if i doesn't exist on c, False if i does exist on c.
    EXAMPLES: -
-}
legalInCol :: Cell -> Coord -> Grid -> Bool
legalInCol Empty _ _ = True
legalInCol (Input i) (r, c) grid = checkCol (Input i) c 1 grid

{- checkCol (Input i) x acc grid
Checks if (Input i) is equal to any of the cells on the col x.
    RETURNS: True if (Input i) isn't equal to any cell on the col x, False if (Input i) is equal to any cell on the col x.
    VARIANT: acc
    EXAMPLES: -
-}
checkCol :: Cell -> Int -> Int -> Grid -> Bool
checkCol (Input i) x acc grid
    | 9 < acc = True
    | getElem acc x grid == Empty = checkCol (Input i) x (acc + 1) grid
    | (Input i) == getElem acc x grid = False
    | otherwise = checkCol (Input i) x (acc + 1) grid


{- step dir game
Transforms a game state according to the argument direction. If the resulting Coord indices are not 0 < (r, c) <= 9, 
returns the corresponding coord at the opposite side of a 9x9 Matrix.
    RETURNS:    coord with one of the components changed according to the following chart:
                Up  | Down | Left | Right
                r-1 |  r+1 | c-1  |  c+1
    EXAMPLES:   step Up (1, 5)      == (9, 5)
                step Left (2, 7)    == (1, 7)
                step Down (4, 4)    == (5, 4)
                step Right (8, 9)   == (8, 1)
                
-}
step :: Dat.Direction -> Game -> Game
step direction game = 
    (\(r, c) -> game {focusedCell = (r, c)}) $ case (direction, (r, c)) of 
            (Up, (1, c))      -> (9, c)
            (Up, (r, c))      -> (r-1, c)
            (Down, (9, c))    -> (1, c)
            (Down, (r, c))    -> (r+1, c)
            (Left, (r, 1))    -> (r, 9)
            (Left, (r, c))    -> (r, c-1)
            (Right, (r, 9))   -> (r, 1)
            (Right, (r, c))   -> (r, c+1)
            where
                (r, c) = focusedCell game

writeGridToFile :: Show a => Matrix a -> FilePath -> IO ()
writeGridToFile input filePath = do
  file <- openFile filePath WriteMode
  hPrint file (toList input)
  hClose file

clearFile :: FilePath -> IO ()
clearFile filePath = writeFile filePath ""

readGridFromFile :: FilePath -> IO ()
readGridFromFile filePath = do
  file <- openFile filePath ReadMode
  contents <- hGetContents file
  print (contents)

--Problems:
--1. readFromFile is not working (hGetContents is used in the wrong way)
--2. for writeToFile to work, the input has to be a variable that contains the most recent updated version of the input
-- grid. (Not a problem really)