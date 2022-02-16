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

testMatrix = matrix 9 9 (\(r, c) -> Empty)

{- insert (input i) (r, c) grid
Inserts i into grid at row number r and column number c if the value is within the given boundary.
RETURNS: the updated version of grid
EXAMPLE: -
-}
insert :: Cell -> Coord -> Grid -> Grid
insert (Input i) (r, c) grid
    | 0 < i && i <= 9 && 1 <= r && r <= 9 && 1 <= c && c <= 9 = setElem (Input i) (r, c) grid
    | otherwise = grid


delete ::  Coord -> Grid -> Grid
delete = undefined 


legalInSubGrid :: Cell -> Coord -> Grid -> Bool
legalInSubGrid = undefined 


listSubGrid :: Coord -> [Coord]
listSubGrid = undefined 


legalInRow :: Cell -> Coord -> Grid -> Bool
legalInRow = undefined 


legalInCol :: Cell -> Coord -> Grid -> Bool
legalInCol = undefined 