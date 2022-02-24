module Types where

import Data.Matrix

{- The value of a given cell in a sudoku-grid
Lock x represents a number in the grid that is predefined, and thus immutable,
Input x represents a number in the grid that was input by the player.
Empty is an empty cell
    INVARIANT:  0 < x <= 10 where x == (Input x) || (Lock x)
-}
data Cell = Lock Int Coord    |
            Input Int Coord   |
            Note [Int] Coord  |
            Empty Coord      deriving (Eq, Show, Read) --Cell has a locked value (predefined), input value (input by user) or is empty

type Grid       = Matrix Cell --Matrix of cell values

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