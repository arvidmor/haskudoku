module Dat where
import Brick
import Data.Matrix

--Build matrix out of these cells
data Cell = Lock Int    |
            Input Int   |
            Empty       deriving Show --Cell has a locked value (predefined), input value (input by user) or is empty

type Grid       = Matrix Cell
type Coord      = (Int, Int)    --(Row index, Column index)

data Direction  = Up | Down | Left | Right

newSudokuMatrix = matrix 9 9 (\(r, c) -> Empty)

{- insert (input i) (r, c) grid
Inserts i into grid at row number r and column number c if the value is within the given boundary.
RETURNS: the updated version of grid
Examples: -
-}
insert :: Cell -> Coord -> Grid -> Grid
insert (Input i) (r, c) grid
    | 0 < i && i <= 9 && 1 <= r && r <= 9 && 1 <= c && c <= 9 = setElem (Input i) (r, c) grid
    | otherwise = grid

{- delete (r, c) grid
Deletes a value from position (r, c) in grid
RETURNS: the updated version of grid
Examples: -
-}
delete ::  Coord -> Grid -> Grid
delete (r, c) grid
    | 1 <= r && r <= 9 && 1 <= c && c <= 9 = setElem Empty (r, c) grid
    | otherwise = grid


legalInSubGrid :: Cell -> Coord -> Grid -> Bool
legalInSubGrid = undefined 

{- listSubGrid (r, c)
Creates a list of every coordinate that exists in the same 3x3 sub-grid as (r, c)
PRE: 0 < r <= 9, 0 < c <= 9
RETURNS: a list of every coordinate that exists in the same 3x3 sub-grid as (r, c)
Examples: listSubGrid (1, 5) = [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
          listSubGrid (7, 9) = [(7,7),(7,8),(7,9),(8,7),(8,8),(8,9),(9,7),(9,8),(9,9)]
-}
listSubGrid :: Coord -> [Coord]
listSubGrid (r, c) = [(x, y) | x <- (coordList r), y <- (coordList c)] --Inspiration från StackOverflow (https://stackoverflow.com/questions/32093912/all-combinations-of-elements-of-two-lists-in-haskell)

{- coordList x
Creates a list from: 1-3 if x ∈ [1..3], 4-6 if x ∈ [4..6], 7-9 if x ∈ [7..9]
RETURNS: a list from: [1..3], [4..6] or [7..9]
Examples: coordList 3 = [1,2,3]
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