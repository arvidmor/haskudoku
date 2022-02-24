module Main (main) where

import Types
import UI
import FileIO
import Brick



main :: IO ()
main = do
    menuState <- defaultMain menuApp menuDialog
    case getChoice menuState of 
        Just 0 -> do 
            putStr "Filename: "
            file <- getLine
            gameState <- loadGrid ("Puzzles/"++file)
            endGame <- defaultMain app gameState
            return ()
        Just 1 -> do 
            endGame <- defaultMain editorApp emptyGame
            saveFileLoop endGame
            return ()
        Just 2 -> do
            putStrLn $ unlines
                ["1: Loads a previously saved game", 
                "2: Starts the editor. Use this if you want to create an own sudoku to play later.", 
                "3: Displays this message", 
                "4: Quit the game"
                ]
            main
        Nothing -> return ()
        _   -> main
