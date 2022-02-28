module Types where

import Data.Matrix

{- The value of a given cell in a sudoku-grid
Lock x c represents a number in the grid at the coordinates c that is predefined, and thus immutable.
Input x c represents a number in the grid at the coordinates c that is input by the player.
Note xs c represents a list of numbers in the grid at the coordinates c.
Empty is an empty cell
    INVARIANT:  0 < x < 10
                xs can only contain numbers between 1 and 0, and can't contain duplicate numbers.
-}
data Cell = Lock Int Coord    |
            Input Int Coord   |
            Note [Int] Coord  |
            Empty Coord      deriving (Eq, Show, Read) --Cell has a locked value (predefined), input value (input by user) or is empty

{- A matrix containing all the cells of a sudoku puzzle
A grid is a matrix where each element is a cell.
    INVARIANT: The size of a grid is a 9 x 9 matrix.
-}
type Grid = Matrix Cell

{- The coordinates of a cell in a grid
In the coordinates (x, y), x represents the row number and y represents the column number.
    INVARIANT: 0 < x < 10   &&    0 < y < 10
-}
type Coord = (Int, Int)    --(Row index, Column index)

{- Indicating direction when navigating the grid.
Up represents moving upwards in the grid.
Down represents moving downwards in the grid.
Left represents movin to the left in the grid.
Right represents moving to the right in the grid.
-}
data Direction  = Up | Down | Left | Right  deriving Show

{- State of the current game.
grid represents the grid.
focusedCell is the coordinates of the cell that the player is currently at.
complete is a puzzle complete flag.
-}
data Game = Game {
    grid        :: Grid,
    focusedCell :: Coord,
    complete    :: Bool
} deriving Show

type Name = ()