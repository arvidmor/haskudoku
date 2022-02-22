module Types where

import Data.Matrix

data Cell = Lock Int Coord    |
            Input Int Coord   |
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