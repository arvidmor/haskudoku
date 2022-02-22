module FileIO where

import Grid
import Types
import UI
import Data.Matrix (fromLists, toLists)
import System.IO


--File IO by Evan Relf, with some refactoring for our implementation of the grid

importGrid :: String -> Game
importGrid = (\g -> Game (fromLists g) (5, 5) False) . read

exportGrid :: Game -> String
exportGrid game = show (toLists (grid game))

saveGrid :: Game -> FilePath -> IO ()
saveGrid game filename = writeFile filename (exportGrid game)

loadGrid :: FilePath -> IO Game
loadGrid filename = importGrid <$> readFile filename
