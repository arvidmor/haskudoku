import Types
import UI
    ( menuApp,
      fileBrowserApp,
      editorApp,
      gameApp,
      menuDialog,
      getChoice,
      fileTypeFilter,
      fileBrowser,
      emptyGame,
      saveDialog,
      saveMenuApp )
import FileIO (loadGame, saveFileLoop)
import Brick (defaultMain)
import Brick.Widgets.FileBrowser(fileBrowserSelection, setFileBrowserEntryFilter, FileInfo(fileInfoFilename))



main :: IO ()
main = do
    menuState <- defaultMain menuApp menuDialog
    case getChoice menuState of 
        Just 0  -> do 
            initBrowserState <- fileBrowser
            browserState <- defaultMain fileBrowserApp (setFileBrowserEntryFilter fileTypeFilter initBrowserState)
            let choice = fileBrowserSelection browserState
            case choice of 
                (file:files)  -> do 
                        gameState <- loadGame ("Puzzles/" ++ fileInfoFilename file)
                        endGame <- defaultMain gameApp gameState
                        saveYN <- defaultMain saveMenuApp saveDialog
                        if getChoice saveYN == Just 0 then do 
                                       saveFileLoop endGame
                                       main
                        else  main
                []      -> main
        Just 1  -> do 
            endGame <- defaultMain editorApp emptyGame
            saveYN <- defaultMain saveMenuApp saveDialog
            case getChoice saveYN of 
                Just 0  -> do 
                        saveFileLoop endGame
                        main
                Just 1  -> main
                Nothing -> return ()
                _       -> return ()
            
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
