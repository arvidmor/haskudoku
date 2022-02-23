module Grid where
import Types
import Solver
import Data.Matrix
import Prelude hiding (Right, Left)

--GRID OPERATIONS


{- isLocked grid coord 
Checks if a cell in a grid is Locked
    RETURNS:    True iff the cell at argument coords is Lock
-}
isLocked :: Grid -> Coord -> Bool
isLocked g (r, c) = let cell = getElem r c g in
    case cell of 
        (Lock _ _)  -> True
        Input _ _   -> False
        Note _ _    -> False
        Empty _     -> False

{- insert (input i) (r, c) grid
Inserts i into grid at row number r and column number c if the value is within the given boundary.
    RETURNS: the updated version of grid
    EXAMPLES: -
-}
insert :: Cell -> Coord -> Game -> Game
insert (Empty _) (r, c) game            = game {grid = setElem (Empty (r, c)) (r, c) (grid game)}
insert (Lock i _) (r, c) game           = game {grid = setElem (Lock i (r, c)) (r, c) (grid game)}
insert (Input i _) (r, c) game 
    | isLocked (grid game) (r, c)   = game
    | otherwise                     = game {grid = setElem (Input i (r, c)) (r, c) (grid game)}
insert (Note xs _) (r, c) game = game {grid = setElem (Note xs (r, c)) (r, c) (grid game)}


{- delete (r, c) grid
Deletes a value from position (r, c) in grid
    RETURNS: the updated version of grid
    EXAMPLES: -
-}
delete ::  Coord -> Game -> Game
delete (r, c) game
    | isLocked (grid game) (r, c)   = game
    | otherwise                     = game {grid = setElem (Empty (r, c)) (r, c) (grid game)}

deleteLocked ::  Coord -> Game -> Game 
deleteLocked (r, c) game
    = game {grid = setElem (Empty (r, c)) (r, c) (grid game)}


box :: Int -> Game -> Matrix Cell
box n game = 
    case n of 
    1   -> submatrix 1 3 1 3 (grid game)
    2   -> submatrix 1 3 4 6 (grid game)
    3   -> submatrix 1 3 7 9 (grid game)
    4   -> submatrix 4 6 1 3 (grid game)
    5   -> submatrix 4 6 4 6 (grid game)
    6   -> submatrix 4 6 7 9 (grid game)
    7   -> submatrix 7 9 1 3 (grid game)
    8   -> submatrix 7 9 4 6 (grid game)
    9   -> submatrix 7 9 7 9 (grid game)

step :: Direction -> Game -> Game
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