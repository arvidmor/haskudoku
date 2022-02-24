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