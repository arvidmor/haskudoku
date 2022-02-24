module Solver where
import Brick hiding (Down, Up, Direction)
import Data.Matrix
import Prelude hiding (Right, Left)
import System.IO
import Types
{- The value of a given cell in a sudoku-grid
Lock x represents a number in the grid that is predefined, and thus immutable,
Input x represents a number in the grid that was input by the player.
Empty is an empty cell
    INVARIANT:  0 < x <= 10 where x == (Input x) || (Lock x)
-}

newSudokuMatrix = matrix 9 9 (\(r, c) -> Empty (r, c))

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

{- legalInCol (Input i) (r, c) grid
Checks if i exists on the column c.
    RETURNS: True if i doesn't exist on c, False if i does exist on c.
    EXAMPLES: -
-}
legalInCol :: Cell -> Game -> Bool -- Necessary to pattern match for Note here?
legalInCol (Empty _) _           = True
legalInCol (Input i (r, c)) game = checkCol (Input i (r, c)) 1 game

{- checkCol (Input i) x acc grid
Checks if (Input i) is equal to any of the cells on the col x.
    RETURNS: True if (Input i) isn't equal to any cell on the col x, False if (Input i) is equal to any cell on the col x.
    VARIANT: acc
    EXAMPLES: -
-}
checkCol :: Cell -> Int -> Game -> Bool
checkCol (Input i (r, c)) acc game
    | 9 < acc                                                   = True
    | getElem acc c (grid game) == Empty (acc, c)               = checkCol (Input i (r, c)) (acc + 1) game
    | (r, c) == getCoordFromCell (getElem acc c (grid game))    = checkCol (Input i (r, c)) (acc + 1) game
    | i == getIntFromCell (getElem acc c (grid game))           = False
    | otherwise                                                 = checkCol (Input i (r, c)) (acc + 1) game

--Gets the int from the cell data-type.
--RETURNS 0 IF THE CELL IS EMPTY OR CONTAINS NOTES
getIntFromCell :: Cell -> Int
getIntFromCell (Input i _)  = i
getIntFromCell (Lock i _)   = i
getIntFromCell (Empty _)    = 0
getIntFromCell (Note _ _)   = 0

getCoordFromCell :: Cell -> Coord
getCoordFromCell (Input _ (r, c))   = (r, c)
getCoordFromCell (Lock _ (r, c))    = (r, c)
getCoordFromCell (Empty (r, c))     = (r, c)
getCoordFromCell (Note _ (r,c))     = (r, c)


--legalInput :: Cell -> Game -> Bool
--legalInput cell game = legalInCol cell game && legalInRow cell game && legalInSubGrid cell game

{- box n game
Creates a submatrix corresponding to the n'th box of the sudoku-grid
    RETURNS:    the n'th box of the grid in the current state game
    EXAMPLES:   
-}


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


{- writeGridToFile input filePath
Writes input to the file with the file path filePath
    RETURNS: Nothing
    EXAMPLES: -
-}
writeGridToFile :: Show a => a -> FilePath -> IO ()
writeGridToFile input filePath = do
  file <- openFile filePath WriteMode
  hPrint file input
  hClose file

{- clearFile filePath
Clears the file with file path filePath.
    RETURNS: Nothing
    EXAMPLES: -
-}
clearFile :: FilePath -> IO ()
clearFile filePath = writeFile filePath ""

{- stringToMatrix str
Converts a list of cells in string-format to a Matrix cell.
    RETURNS: the string str as a Matrix Cell
    EXAMPLES: -
-}

-- stringToMatrix :: String -> Grid
-- stringToMatrix str = fromList 9 9 (stringToMatrixAux str)

{- stringToMatrixAux str
Creates a list of cells from a string containing key-words of type cell.
    RETURNS: a list of cells based on the cells inside the string str.
    VARIANT: amount of words in str (fix this variant)
    EXAMPLES: -
-}

-- stringToMatrixAux :: String -> [Cell]
-- stringToMatrixAux "" = []
-- stringToMatrixAux str@(x:xs)
--     | [x] == "[" || [x] == "]" || [x] == "," = stringToMatrixAux xs
--     | [x] == "E"                             = [Empty] ++ stringToMatrixAux (shortenString (x:xs))
--     | [x] == "L"                             = [Lock (getNr (x:xs) 1 6)] ++ stringToMatrixAux (shortenString (x:xs))
--     | [x] == "I"                             = [Input (getNr (x:xs) 1 7)] ++ stringToMatrixAux (shortenString (x:xs))
--     | otherwise                              = [] ++ stringToMatrixAux (shortenString (x:xs))

{- getNr str acc lim
Gets a specific character in a string and returns it as an int.
    RETURNS: the character indexed (lim - acc) in str as an int
    VARIANT: (lim - acc)
    EXAMPLES: -
-}
getNr :: String -> Int -> Int -> Int
getNr str@(x:xs) acc lim
    | acc == lim = (read [x] :: Int)
    | otherwise  = getNr xs (acc + 1) lim

{- shortenString str
Removes every character in a string until a "," or a "]" appears in the string
    RETURNS: the remaining part of the string str after it has been shortened
    VARIANT: (length str) until , or ] is found. (fix this variant)
    EXAMPLES: shortenString "hehe, hoho" = " hoho"
-}
shortenString :: String -> String
shortenString str@(x:xs)
    | [x] == "," || [x] == "]" = xs
    | otherwise                = shortenString xs