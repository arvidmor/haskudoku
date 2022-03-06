module Grid where
import Types (Game(..), Direction(..), Coord, Grid, Cell(..))
import Solver (getNotesFromCell)
import Data.Matrix (getElem, matrix, setElem)
import Prelude hiding (Right, Left)

newSudokuMatrix = matrix 9 9 (\(r, c) -> Empty (r, c))

--GRID OPERATIONS
{- undo game
Reverts the latest operation on current game state.
    RETURNS:    if previous gamestate == Nothing then game 
                else game where grid == previous game, and previous == previous (previous game)
-}
undo :: Game -> Game
undo game = case previous game of 
    Nothing     -> game
    Just pGame  -> game {grid = grid pGame, previous = previous pGame}

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

{- insert cell game
Inserts a cell value into the grid of a game state if the value at that position is not locked. 
Inserting a Note calls toggleNote recursively for all integers in the list of the note. 
    PRE:        if cell == (Note nums coord) then nums must only contain integers from 1 to 9
    RETURNS:    if the cell at the coordinate value of cell is locked OR cell == (Note [] coord) then game 
                else game with cell inserted in the grid
-}
insert :: Cell -> Game -> Game
insert (Empty (r, c))   game        = game {grid = setElem (Empty (r, c)) (r, c) (grid game)}
insert (Lock i (r, c))  game        = game {grid = setElem (Lock i (r, c)) (r, c) (grid game)}
insert (Input i (r, c)) game
    | isLocked (grid game) (r, c)   = game
    | otherwise                     = game {grid = setElem (Input i (r, c)) (r, c) (grid game)}
--VARIANT:  length xs
insert (Note [] (r, c)) game        = game
insert (Note (x:xs) (r, c)) game    = insert (Note xs (r, c)) (toggleNote x (r, c) game)

{- toggleNote num (r, c) game
Inserts a note into the grid of a game state if the value at that position is not locked.
    PRE:        1 <= num <= 9
                r, c <- [1, 2, 3, 4, 5, 6, 7, 8, 9]
    RETURNS:    game with (Note num (r, c)) toggled in the grid of game
-}

toggleNote :: Int -> Coord -> Game -> Game
toggleNote num (r, c) game = let notes = getNotesFromCell (getElem r c (grid game)) in
    case isNote (grid game) (r, c) of
    0    -> game
    1    -> game {grid = setElem (Note [num] (r, c)) (r, c) (grid game)}
    2   | [num] == notes    -> game {grid = setElem (Empty (r, c)) (r, c) (grid game)}
        | num `elem` notes  -> game {grid = setElem (Note (filter (num /=) notes) (r, c)) (r, c) (grid game)}
        | otherwise         -> game {grid = setElem (Note (num : notes) (r, c)) (r, c) (grid game)}
    _    -> game

{- delete (r, c) game
Deletes a value from position (r, c) in grid if the cell at that position is not locked
    PRE:        r, c <- [1, 2, 3, 4, 5, 6, 7, 8, 9]
    RETURNS:    iff (the cell at (r, c) in the grid of game) == (Lock _ (r, c)) then game
                else game with (the cell at (r, c)) == Empty (r, c)
    EXAMPLES: -
-}
delete ::  Coord -> Game -> Game
delete (r, c) game
    | isLocked (grid game) (r, c)   = game
    | otherwise                     = game {grid = setElem (Empty (r, c)) (r, c) (grid game)}

{- deleteLocked (r, c) game
Removes a locked cell from a grid in a game state
    PRE:        r, c <- [1, 2, 3, 4, 5, 6, 7, 8, 9]
    RETURNS:    game with (the cell at (r, c)) == Empty (r, c))
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