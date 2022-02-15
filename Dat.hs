module Dat where
import Brick

data Cell = Lock Int    |
            Input Int   |
            Empty       deriving Show --Cell has a locked value (predefined), input value (input by user) or is empty

type Row        = [Cell]
type Grid       = [Row]
type Pos        = (Int, Int)    --(Row index, Column index)

data Direction  = Up | Down | Left | Right

emptyGrid :: Grid
emptyGrid = replicate 9 (replicate 9 Empty)

insert :: Int -> Pos -> Grid -> Grid
insert num (_, col) _   | col > 9 || col < 1    = error "Invalid row index"
insert num (1, col) (r:rs)                      = insert' (Input num) col r:rs
insert num (row, col) (r:rs)                    = r : insert num (row-1, col) rs

insert' :: Cell -> Int -> Row -> Row
insert' num index row   | index < 1 || index > 9   = error "Invalid column index"
insert' num 1 (x:xs)    | isLocked x                = x:xs
                        | otherwise                 = num:xs
insert' num col (x:xs)                              = x : insert' num (col-1) xs

isLocked :: Cell -> Bool
isLocked (Input _)    = False
isLocked Empty        = False
isLocked (Lock _)     = True


sampleGrid :: Grid
sampleGrid = [
    [Lock 4, Empty, Lock 1, Lock 2, Lock 9, Empty, Empty, Lock 7, Lock 5],
    [Lock 2, Empty, Empty, Lock 3, Empty, Empty, Lock 8, Empty, Empty],
    [Empty, Lock 7, Empty, Empty, Lock 8, Empty, Empty, Empty, Lock 6],
    [Empty, Empty, Empty, Lock 1, Empty, Lock 3, Empty, Lock 6, Lock 2],
    [Lock 1, Empty, Lock 5, Empty, Empty, Empty, Lock 4, Empty, Lock 3],
    [Lock 7, Lock 3, Empty, Lock 6, Empty, Lock 8, Empty, Empty, Empty],
    [Lock 6, Empty, Empty, Empty, Lock 2, Empty, Empty, Lock 3, Empty],
    [Empty, Empty, Lock 7, Empty, Empty, Lock 1, Empty,Empty, Lock 4],
    [Lock 8, Lock 9, Empty, Empty, Lock 6, Lock 5, Lock 1, Empty, Lock 7]
    ]
