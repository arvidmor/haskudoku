module FileIO where

import Grid
import Types
import UI
import Data.Matrix (fromLists, toLists)
import System.IO


--File IO by Evan Relf, with some refactoring for our implementation of the grid

{- importGrid str
Takes a string and transforms the string into a game.
    RETURNS: the string str as a game.
    EXAMPLES: -
-}
importGrid :: String -> Game
importGrid = (\g -> Game (fromLists g) (5, 5) False) . read

{- exportGrid game
Exports game to a list-representation of game.
    RETURNS: the list-version of game.
    EXAMPLES: -
-}
exportGrid :: Game -> String
exportGrid game = show (toLists (grid game))

{- saveGrid game filename
Saves a gamestate to a file
    SIDE-EFFECTS: writes game to the file with the name filename
    RETURNS: Nothing
    EXAMPLES: -
-}
saveGrid :: Game -> FilePath -> IO ()
saveGrid game filename = writeFile filename (exportGrid game)

{-loadGrid filename
Loads the content of a file
    SIDE-EFFECTS: reads a string from a file with the name filename and reads it as a game.
    RETURNS: Nothing
    EXAMPLES: -
-}
loadGrid :: FilePath -> IO Game
loadGrid filename = importGrid <$> readFile filename

{-saveFileLoop game
Saves a gamestate to any file located in the directory "puzzles"
    SIDE-EFFECTS: writes the contents of game to a file within the directory Puzzles.
    RETURNS: Nothing
    EXAMPLES: -
-}
saveFileLoop :: Game -> IO ()
saveFileLoop game = do 
    putStr "Filename: "
    fileName <- getLine
    saveGrid game ("Puzzles/"++fileName)