module Main (main) where

import Types
import UI
import FileIO
import Brick



main :: IO ()
main = do
    putStrLn $ unlines 
        ["Welcome!", "1) Load game", "2) Start editor", "3) Help", "q) Quit"]
    putStr ">"
    choice <- getLine
    case choice of 
        "1" -> do 
            putStr "Filename: "
            file <- getLine
            gameState <- loadGrid file
            endGame <- defaultMain app gameState
            return ()
        "2" -> do 
            endGame <- defaultMain editorApp emptyGame
            saveFileLoop endGame
            return ()
        "3" -> do
            putStrLn $ unlines
                ["1: Loads a previously saved game", 
                "2: Starts the editor. Use this if you want to create an own sudoku to play later.", 
                "3: Displays this message", 
                "4: Quit the game"
                ]
            main
        "q" -> return ()
        _   -> main
