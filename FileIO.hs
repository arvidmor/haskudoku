module FileIO where

import Types (Game(..))
import Data.Matrix (fromLists, toLists)
import System.IO (hFlush, stdout)


--File IO by Evan Relf, with some refactoring for our implementation of the grid

{- importGrid str
Takes a string and transforms the string into a game.
    RETURNS: a Game with record fields as follows: {grid == read str, focusedCell == (5, 5), complete == False, previous == Nothing}.
-}
importGrid :: String -> Game
importGrid = (\g -> Game (fromLists g) (5, 5) False Nothing) . read

{- exportGrid game
Exports game to a list-representation of game.
    RETURNS: (grid game) converted into a list
-}
exportGrid :: Game -> String
exportGrid game = show (toLists (grid game))

{- saveGame game filename
Saves a grid of a game state to a file
    SIDE-EFFECTS: writes (grid game) as a list to the file with the name filename
    RETURNS: game
-}
saveGame :: Game -> FilePath -> IO ()
saveGame game filename = writeFile filename (exportGrid game)

{-loadGame filename
Loads the content of a file
    SIDE-EFFECTS: reads a string from a file with the name filename
    RETURNS: a Game with record fields as follows: {grid == read str, focusedCell == (5, 5), complete == False, previous == Nothing}.
-}
loadGame :: FilePath -> IO Game
loadGame filename = importGrid <$> readFile filename

{-saveFileLoop game
Saves a gamestate to any file located in the directory "puzzles"
    SIDE-EFFECTS:   writes the contents of game to a file within the directory Puzzles.
                    prints "Filename: " in the terminal
                    reads input from user
    RETURNS: game
-}
saveFileLoop :: Game -> IO ()
saveFileLoop game = do 
    putStr "Filename: "
    hFlush stdout
    fileName <- getLine
    saveGame game ("Puzzles/"++fileName)