module Dat where
import Brick hiding (Down, Up)
import Data.Matrix
import Prelude hiding (Right, Left)

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

{- insert (input i) (r, c) grid
Inserts i into grid at row number r and column number c if the value is within the given boundary.
    RETURNS: the updated version of grid
    EXAMPLES: -
-}
insert :: Cell -> Coord -> Grid -> Grid
insert (Input i) (r, c) grid
    | 0 < i && i <= 9 && 1 <= r && r <= 9 && 1 <= c && c <= 9 = setElem (Input i) (r, c) grid
    | otherwise = grid

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
    | Input i == getElem (fst x) (snd x) grid = False
    | otherwise = legalInSubGrid (Input i) xs grid

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

legalInRow :: Cell -> Coord -> Grid -> Bool
legalInRow = undefined 


legalInCol :: Cell -> Coord -> Grid -> Bool
legalInCol = undefined 


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