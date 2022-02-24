module Main (main) where

import Types
import UI
import FileIO
import Brick



main :: IO ()
main = do
    menuState <- defaultMain menuApp menuDialog
    case getChoice menuState of 
        Just 0  -> do 
            putStr "Filename: "
            file <- getLine
            gameState <- loadGrid ("Puzzles/"++file)
            endGame <- defaultMain app gameState
            return ()
        Just 1  -> do 
            endGame <- defaultMain editorApp emptyGame
            saveFileLoop endGame
            return ()
        Just 2  -> do
            putStrLn $ unlines
                ["Load: Loads a previously saved game", 
                "Editor: Starts the editor. Use this if you want to create an own sudoku to play later.", 
                "Help: Displays this message", 
                "Quit: Quit the game"
                ]
            putStr "Press enter to go back> "
            userIsDone <- getLine 
            main
        Just 3  -> do return ()
        Nothing -> do return ()
        _   -> main
