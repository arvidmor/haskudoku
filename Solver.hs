module Solver where
import Brick hiding (Down, Up, Direction)
import Data.Matrix
import Prelude hiding (Right, Left)
import System.IO
import Types



{- legalInSubGrid (Input i) lst grid
Checks if i exists inside grid's subgrid lst
    RETURNS: True if i doesn't exist in the subgrid lst, False if i does exist in the subgrid lst.
    VARIANT: length lst
    EXAMPLES: -
-}
legalInSubGrid :: Cell -> [Coord] -> Game -> Bool
legalInSubGrid _ [] _                                     = True
legalInSubGrid (Empty _) _ _                              = True
legalInSubGrid (Note _ _) _ _                             = True
legalInSubGrid (Input i coord) lst@(x:xs) game
    | coord == x = legalInSubGrid (Input i coord) xs game
    | i == getIntFromCell (uncurry getElem x (grid game)) = False
    | otherwise                                           = legalInSubGrid (Input i coord) xs game

{- listSubGrid (r, c)
Creates a list of every coordinate that exists in the same 3x3 sub-grid as (r, c)
    PRE: 0 < r <= 9, 0 < c <= 9
    RETURNS: a list of every coordinate that exists in the same 3x3 sub-grid as (r, c)
    EXAMPLES: listSubGrid (1, 5) = [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
              listSubGrid (7, 9) = [(7,7),(7,8),(7,9),(8,7),(8,8),(8,9),(9,7),(9,8),(9,9)]
-}
listSubGrid :: Coord -> [Coord]
listSubGrid (r, c) = [(x, y) | x <- coordList r, y <- coordList c] --Inspiration from StackOverflow (https://stackoverflow.com/questions/32093912/all-combinations-of-elements-of-two-lists-in-haskell)

{- coordList x
Creates a list from: 1-3 if x ∈ [1..3], 4-6 if x ∈ [4..6], 7-9 if x ∈ [7..9]
    RETURNS:    a list from: [1..3], [4..6] or [7..9]
    EXAMPLES:   coordList 3 = [1,2,3]
                coordList 2 = [1,2,3]
                coordList 8 = [7,8,9]
-}
coordList :: Int -> [Int]
coordList x
    | 0 < x && x <= 3 = [1..3]
    | 3 < x && x <= 6 = [4..6]
    | 6 < x && x <= 9 = [7..9]
    | otherwise       = []

{- legalInRow (Input i) (r, c) grid
Checks if i exists on the row r.
    RETURNS: True if i doesn't exist on r, False if i does exist on r.
    EXAMPLES: -
-}
legalInRow :: Cell -> Game -> Bool
legalInRow (Empty _) _           = True
legalInRow (Lock i (r, c)) game  = checkRow (Input i (r, c)) 1 game
legalInRow (Input i (r, c)) game = checkRow (Input i (r, c)) 1 game

{- checkRow (Input i) x acc grid
Checks if (Input i) is equal to any of the cells on the row x.
    RETURNS: True if (Input i) isn't equal to any cell on the row x, False if (Input i) is equal to any cell on the row x.
    VARIANT: acc
    EXAMPLES: -
-}
checkRow :: Cell -> Int -> Game -> Bool
checkRow (Input i (r, c)) acc game
    | 9 < acc                                         = True
    | getElem r acc (grid game) == Empty (r, acc)       = checkRow (Input i (r, c)) (acc + 1) game
    | (r, c) == getCoordFromCell (getElem r acc (grid game)) = checkRow (Input i (r, c)) (acc + 1) game -- if compared with self, move on
    | i == getIntFromCell (getElem r acc (grid game)) = False
    | otherwise                                       = checkRow (Input i (r, c)) (acc + 1) game

--Check if a cell is legal
legalInput :: Cell -> Game -> Bool
legalInput cell game = 
    legalInSubGrid cell (listSubGrid (getCoordFromCell cell)) game && legalInRow cell game && legalInCol cell game

{- legalInCol (Input i) (r, c) grid
Checks if i exists on the column c.
    RETURNS: True if i doesn't exist on c, False if i does exist on c.
    EXAMPLES: -
-}
legalInCol :: Cell -> Game -> Bool -- Necessary to pattern match for Note here?
legalInCol (Empty _) _           = True
legalInCol (Lock i (r, c)) game  = checkCol (Input i (r, c)) 1 game
legalInCol (Input i (r, c)) game = checkCol (Input i (r, c)) 1 game

{- checkCol (Input i) x acc grid
Checks if (Input i) is equal to any of the cells on the col x.
    RETURNS: True if (Input i) isn't equal to any cell on the col x, False if (Input i) is equal to any cell on the col x.
    VARIANT: acc
    EXAMPLES: -
-}
checkCol :: Cell -> Int -> Game -> Bool
checkCol (Input i (r, c)) acc game
    | 9 < acc                                         = True
    | getElem acc c (grid game) == Empty (acc, c)     = checkCol (Input i (r, c)) (acc + 1) game
    | (r, c) == getCoordFromCell (getElem acc c (grid game)) = checkCol (Input i (r, c)) (acc + 1) game
    | i == getIntFromCell (getElem acc c (grid game)) = False
    | otherwise                                       = checkCol (Input i (r, c)) (acc + 1) game

--Gets the int from the cell data-type.
--RETURNS 0 IF THE CELL IS EMPTY OR CONTAINS NOTES
getIntFromCell :: Cell -> Int
getIntFromCell (Input i _)  = i
getIntFromCell (Lock i _)   = i
getIntFromCell (Empty _)    = 0
getIntFromCell (Note _ _)   = 0

--Gets the coord-value from a cell
getCoordFromCell :: Cell -> Coord
getCoordFromCell (Input _ (r, c))   = (r, c)
getCoordFromCell (Lock _ (r, c))    = (r, c)
getCoordFromCell (Empty (r, c))     = (r, c)
getCoordFromCell (Note _ (r,c))     = (r, c)

getNotesFromCell :: Cell -> [Int]
getNotesFromCell cell =
    case cell of
        (Note xs _) -> xs
        (Lock _ _)  -> []
        (Input _ _) -> []
        (Empty _)   -> []

{- isCompleted game
Checks if the game is finished or not
    RETURNS: True if finished, otherwise False.
    EXAMPLES: -
-}
isCompleted :: Game -> Bool
isCompleted game = checkAllCols game && checkAllRows game && checkAllSubGrids game && isFull game

{- checkAllCols game
Checks if the number in each cell is legal in all cols
    RETURNS: True if all cells are legal, otherwise False.
    EXAMPLES: -
-}
checkAllCols :: Game -> Bool
checkAllCols game = checkAllColsAux game 1 1

{- checkAllColsAux game row col
Goes through every col and checks if every number is legal.
    RETURNS: True if legal, otherwise False.
    EXAMPLES: -
-}
checkAllColsAux :: Game -> Int -> Int -> Bool
checkAllColsAux game row col
    | col == 10 = True
    | otherwise = legalCol game row col && checkAllColsAux game row (col + 1)

{- legalCol game row col
Checks if each cells number is legal in a specific col.
    RETURNS: True if legal, otherwise False.
    EXAMPLES: -
-}
legalCol :: Game -> Int -> Int -> Bool
legalCol game row col
    | row == 10 = True
    | otherwise = legalInCol (getElem row col (grid game)) game && legalCol game (row + 1) col

{- checkAllRows game
Checks if the number in each cell is legal in all rows.
    RETURNS: True if all cells are legal, otherwise False.
    EXAMPLES: -
-}
checkAllRows :: Game -> Bool
checkAllRows game = checkAllRowsAux game 1 1

{- checkAllRowsAux game row col
Goes through every row and checks if every number is legal.
    RETURNS: True if legal, otherwise False.
    EXAMPLES: -
-}
checkAllRowsAux :: Game -> Int -> Int -> Bool
checkAllRowsAux game row col
    | row == 10 = True
    | otherwise = legalRow game row col && checkAllRowsAux game (row + 1) col

{- legalRow game row col
Checks if each cells number is legal in a specific row.
    RETURNS: True if legal, otherwise False.
    EXAMPLES: -
-}
legalRow :: Game -> Int -> Int -> Bool
legalRow game row col
    | col == 10 = True
    | otherwise = legalInRow (getElem row col (grid game)) game && legalRow game row (col + 1)

{- checkAllSubGrids game
Checks if the number of each cell is legal in all sub-grids.
    RETURNS: True if legal, otherwise False.
    EXAMPLES: -
-}
checkAllSubGrids :: Game -> Bool
checkAllSubGrids game = checkAllSubGridsAux game 1

{- checkAllSubGridsAux game boxId
Checks if all boxes are legal.
    RETURNS: True if legal, otherwise False.
    EXAMPLES: -
-}
checkAllSubGridsAux :: Game -> Int -> Bool
checkAllSubGridsAux game boxId
    | boxId == 10 = True
    | otherwise = checkSubGrid (toList (box boxId game)) && checkAllSubGridsAux game (boxId + 1)

{- checkSubGrid lst
Helper-function for checkAllSubGridsAux. Checks if each cell in a specific box is legal.
    RETURNS: True if legal, otherwise False.
    EXAMPLE: -
-}
checkSubGrid :: [Cell] -> Bool
checkSubGrid lst
    | length (uniq [] lst) == 9 = True
    | otherwise = False

{- uniq [] (x:xs)
Removes duplicates from a list so that each item only appears once.
    RETURNS: a list where each item only appears once.
    EXAMPLES: uniq [1,1,1,3,4,5,5,6] = [1,3,4,5,6]
-}
uniq :: Eq a => [a] -> [a] -> [a]                               --From: https://codereview.stackexchange.com/questions/150533/filter-duplicate-elements-in-haskell
uniq x [] = x
uniq [] (a:xs) = uniq [a] xs
uniq x (a:xs) = if a `elem` x then uniq x xs else uniq (a:x) xs

{- isFull game
Checks if the game grid is filled (contains no comments or empty cells)
    RETURNS: True if filled, otherwise False.
    EXAMPLES: -
-}
isFull :: Game -> Bool
isFull game = checkFull game [(x, y) | x <- [1..9], y <- [1..9]]

{- checkFUll game ((r,c):xs)
Checks every coord of the grid and checks if its empty
    RETURNS: True if full, otherwise False.
-}
checkFull :: Game -> [Coord] -> Bool
checkFull game [] = True
checkFull game ((r, c):xs) = case cell of
    (Note xs _) -> False
    (Lock _ _)  -> checkFull game xs
    (Input _ _) -> checkFull game xs
    (Empty _)   -> False
    where cell = getElem r c (grid game) 

{- box n game
Creates a submatrix corresponding to the n'th box of the sudoku-grid
    RETURNS:    the n'th box of the grid in the current state game
    EXAMPLES:
-}
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

{- clearFile filePath
Clears the file with file path filePath.
    RETURNS: Nothing
    EXAMPLES: -
-}
clearFile :: FilePath -> IO ()
clearFile filePath = writeFile filePath ""