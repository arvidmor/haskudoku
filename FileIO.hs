module FileIO where

import Types (Game(..))
import Data.Matrix (fromLists, toLists)
import System.IO (hFlush, stdout)


--File IO by Evan Relf, with some refactoring for our implementation of the grid

{- importGrid str
Takes a string and transforms the string into a game.
    RETURNS: the string str as a game.
    EXAMPLES: -
-}
importGrid :: String -> Game
importGrid = (\g -> Game (fromLists g) (5, 5) False Nothing) . read

{- exportGrid game
Exports game to a list-representation of game.
    RETURNS: the list-version of game.
    EXAMPLES: -
-}
exportGrid :: Game -> String
exportGrid game = show (toLists (grid game))

{- saveGame game filename
Saves a gamestate to a file
    SIDE-EFFECTS: writes game to the file with the name filename
    RETURNS: Nothing
    EXAMPLES: -
-}
saveGame :: Game -> FilePath -> IO ()
saveGame game filename = writeFile filename (exportGrid game)

{-loadGame filename
Loads the content of a file
    SIDE-EFFECTS: reads a string from a file with the name filename and reads it as a game.
    RETURNS: Nothing
    EXAMPLES: -
-}
loadGame :: FilePath -> IO Game
loadGame filename = importGrid <$> readFile filename

{-saveFileLoop game
Saves a gamestate to any file located in the directory "puzzles"
    SIDE-EFFECTS: writes the contents of game to a file within the directory Puzzles.
    RETURNS: Nothing
    EXAMPLES: -
-}
saveFileLoop :: Game -> IO ()
saveFileLoop game = do 
    putStr "Filename: "
    hFlush stdout
    fileName <- getLine
    saveGame game ("Puzzles/"++fileName)