module Grid where
import Types
import Solver
import Data.Matrix
import Prelude hiding (Right, Left)

newSudokuMatrix = matrix 9 9 (\(r, c) -> Empty (r, c))

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

{- isNote g (r, c)
Checks which cell-type exists at a specific coordinate in a grid
    RETURNS: The Int value of the cell with coordinate (r, c) in the grid g.
    EXAMPLES: -
-}
isNote :: Grid -> Coord -> Int
isNote g (r, c) =
    let cell = getElem r c g in
    case cell of
        (Lock _ _)  -> 0
        Input _ _   -> 1
        Note _ _    -> 2
        Empty _     -> 1

{- insert (input i) (r, c) grid
Inserts i into grid at row number r and column number c if the value is within the given boundary.
    RETURNS: the updated version of grid
    EXAMPLES: -
-}
insert :: Cell -> Coord -> Game -> Game
insert (Empty _) (r, c) game        = game {grid = setElem (Empty (r, c)) (r, c) (grid game)}
insert (Lock i _) (r, c) game       = game {grid = setElem (Lock i (r, c)) (r, c) (grid game)}
insert (Input i _) (r, c) game
    | isLocked (grid game) (r, c)   = game
    | otherwise                     = game {grid = setElem (Input i (r, c)) (r, c) (grid game)}
insert (Note [x] _) (r, c) game     = toggleNote x (r, c) game

toggleNote :: Int -> Coord -> Game -> Game
toggleNote num (r, c) game = let notes = getNotesFromCell(getElem r c (grid game)) in
    case isNote (grid game) (r, c) of
    0    -> game
    1    -> game {grid = setElem (Note [num] (r, c)) (r, c) (grid game)}
    2   | [num] == notes    -> game {grid = setElem (Empty (r, c)) (r, c) (grid game)}
        | num `elem` notes  -> game {grid = setElem (Note (filter (num /=) notes) (r, c)) (r, c) (grid game)}
        | otherwise         -> game {grid = setElem (Note (num : notes) (r, c)) (r, c) (grid game)}
    _    -> game

{- delete (r, c) grid
Deletes a value from position (r, c) in grid
    RETURNS: the updated version of grid
    EXAMPLES: -
-}
delete ::  Coord -> Game -> Game
delete (r, c) game
    | isLocked (grid game) (r, c)   = game
    | otherwise                     = game {grid = setElem (Empty (r, c)) (r, c) (grid game)}

{- deleteLocked (r, c) game
Removes a locked cell from a game.
    RETURNS: game without the locked cell at coordinate (r, c)
    EXAMPLES: -
-}
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