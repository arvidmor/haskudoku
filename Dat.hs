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

insert :: Cell -> Coord -> Grid -> Grid
insert = undefined


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