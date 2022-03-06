module UI where
import Solver
    ( box,
      getCoordFromCell,
      getIntFromCell,
      isCompleted,
      isFull,
      legalInput )
import Types (Name, Game(..), Direction(..), Cell(..))
import Grid (delete, deleteLocked, insert, newSudokuMatrix, step, toggleNote, undo)

import Graphics.Vty hiding (Input)

import Brick hiding (Up, Down)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (borderWithLabel, hBorderWithLabel, vBorder)
import Brick.Widgets.Dialog
    ( buttonAttr,
      buttonSelectedAttr,
      dialog,
      dialogSelection,
      handleDialogEvent,
      renderDialog,
      Dialog)
import Brick.Widgets.Border.Style (unicode, unicodeBold, unicodeRounded)

import Data.Matrix (toList)
import Data.List.Split (chunksOf)
import Prelude hiding (Right, Left)

import Brick.Widgets.Table ( renderTable, surroundingBorder, table )
import Brick.Widgets.List (list, renderList, listSelectedAttr)
import Data.List (intersperse, intercalate)
import Brick.Widgets.FileBrowser
    ( actionFileBrowserListNext,
      actionFileBrowserListPrev,
      actionFileBrowserSelectEnter,
      fileBrowserRegularFileAttr,
      fileTypeMatch,
      newFileBrowser,
      renderFileBrowser,
      selectNonDirectories,
      FileBrowser,
      FileInfo,
      FileType(RegularFile) )

-- ATTRIBUTES
lockAttr, inputAttr, noteAttr, focusedAttr, illegalAttr, focusedInputAttr, focusedNoteAttr, focusedIllegalAttr, defaultAttr :: AttrName
logoAttr, incompleteAttr, incorrectAttr, completeAttr, illegalLockAttr :: AttrName
lockAttr            = attrName "Lock"
inputAttr           = attrName "Input"
noteAttr            = attrName "Note"
focusedAttr         = attrName "Focused"
illegalAttr         = attrName "Illegal"
illegalLockAttr     = attrName "IllegalLock"
focusedInputAttr    = attrName "FocusedInput"
focusedIllegalInputAttr = attrName "FocusedIllegalInput"
focusedIllegalLockAttr  = attrName "FocusedIllegalLock"
focusedNoteAttr     = attrName "FocusedNote"
focusedIllegalAttr  = attrName "FocusedIllegal"
defaultAttr         = attrName "Default"
logoAttr            = attrName "Logo"
incompleteAttr      = attrName "Incomplete"
incorrectAttr       = attrName "Incorrect"
completeAttr        = attrName "Complete"


-- ATTRIBUTE MAPS
gameAttrs, menuAttrs, fileBrowserAttrs :: AttrMap
gameAttrs = attrMap defAttr [

    (lockAttr, fg white),
    (defaultAttr, defAttr),
    (inputAttr, fg brightBlue),
    (noteAttr, fg brightGreen),
    (focusedAttr, bg brightBlack),
    (illegalAttr, brightBlue `on` red),
    (illegalLockAttr, white `on` red),
    (focusedInputAttr, brightBlue `on` brightBlack),
    (focusedNoteAttr, brightGreen `on` brightBlack),
    (focusedIllegalInputAttr, brightBlue `on` rgbColor 255 114 111),
    (focusedIllegalLockAttr, white `on` rgbColor 255 114 111),
    (incompleteAttr, fg yellow),
    (incorrectAttr, fg red),
    (completeAttr, fg brightGreen)
    ]

menuAttrs = attrMap defAttr [
    (buttonSelectedAttr,    bg brightBlack),
    (buttonAttr,            fg white),
    (logoAttr,              fg green)
    ]

fileBrowserAttrs = attrMap defAttr [
    (fileBrowserRegularFileAttr,    fg green),
    (listSelectedAttr,              green `on` brightBlack)
    ]

{- App types
The app data type is what brick uses to decide which functions define the behavior for a given app. 
    appDraw:        Which function is used to create the widgets displayed 
    appChooseCursor: where to place the cursor or which function to define cursor behaviour, if any. 
    appHandleEvent:  which funciton defines the event handling
    appStartEvent:  An event to run when the app is initialized
    appAttrMap:     function for of the attribute map for the app 
-}
menuApp :: App (Dialog Int) a Name
menuApp = App {
    appDraw         = drawMenu,
    appChooseCursor = showFirstCursor,
    appHandleEvent  = handleEventMenu,
    appStartEvent   = return,
    appAttrMap      = const menuAttrs
}

saveMenuApp :: App (Dialog Int) a Name
saveMenuApp = App {
    appDraw         = drawSaveMenu,
    appChooseCursor = showFirstCursor,
    appHandleEvent  = handleEventMenu,
    appStartEvent   = return,
    appAttrMap      = const menuAttrs
}

fileBrowserApp :: App (FileBrowser Name) a Name
fileBrowserApp = App {
    appDraw         = drawFileBrowser,
    appChooseCursor = neverShowCursor,
    appHandleEvent  = handleEventFileBrowser,
    appStartEvent   = return,
    appAttrMap      = const fileBrowserAttrs
}

editorApp :: App Game a Name
editorApp = App {
    appDraw         = drawEditor,
    appChooseCursor = neverShowCursor,
    appHandleEvent  = handleEventEditor,
    appStartEvent   = return,
    appAttrMap      = const gameAttrs
}

gameApp :: App Game a Name
gameApp = App {
    appDraw         = drawGame,
    appChooseCursor = neverShowCursor,
    appHandleEvent  = handleEventGame,
    appStartEvent   = return,
    appAttrMap      = const gameAttrs
}

--EVENT HANDLING
--Event handling inspired by Evan Relf: https://github.com/evanrelf/sudoku-tui.git
handleEventGame :: Game -> BrickEvent Name a -> EventM Name (Next Game)
--Quit game
handleEventGame g (VtyEvent (EvKey (KChar 'q') [])) =
    halt g
--Grid operations
handleEventGame g (VtyEvent (EvKey key [])) =
    continue event where
        coord = focusedCell g
        event = case key of
            --Navigation
            KUp         -> step Up g
            KDown       -> step Down g
            KLeft       -> step Left g
            KRight      -> step Right g
            --Input and remove numbers
            (KChar '1') -> insert (Input 1 coord) g {previous = Just g}
            (KChar '2') -> insert (Input 2 coord) g {previous = Just g}
            (KChar '3') -> insert (Input 3 coord) g {previous = Just g}
            (KChar '4') -> insert (Input 4 coord) g {previous = Just g}
            (KChar '5') -> insert (Input 5 coord) g {previous = Just g}
            (KChar '6') -> insert (Input 6 coord) g {previous = Just g}
            (KChar '7') -> insert (Input 7 coord) g {previous = Just g}
            (KChar '8') -> insert (Input 8 coord) g {previous = Just g}
            (KChar '9') -> insert (Input 9 coord) g {previous = Just g}
            (KChar 'u') -> undo g
            KDel        -> delete coord g {previous = Just g}
            KBS         -> delete coord g {previous = Just g}
            --Toggle notes
            (KChar '!') -> toggleNote 1 coord g {previous = Just g}
            (KChar '"') -> toggleNote 2 coord g {previous = Just g}
            (KChar '#') -> toggleNote 3 coord g {previous = Just g}
            (KChar '¤') -> toggleNote 4 coord g {previous = Just g}
            (KChar '%') -> toggleNote 5 coord g {previous = Just g}
            (KChar '&') -> toggleNote 6 coord g {previous = Just g}
            (KChar '/') -> toggleNote 7 coord g {previous = Just g}
            (KChar '(') -> toggleNote 8 coord g {previous = Just g}
            (KChar ')') -> toggleNote 9 coord g {previous = Just g}
            _           -> g
--Everything else
handleEventGame g _ =
    continue g

handleEventEditor :: Game -> BrickEvent Name a -> EventM Name (Next Game)
--Quit Editor
handleEventEditor g (VtyEvent (EvKey (KChar 'q') [])) =
    halt g
--Grid operations
handleEventEditor g (VtyEvent (EvKey key [])) =
    continue event where
        coord = focusedCell g
        event = case key of
            --Navigation
            KUp         -> step Up g
            KDown       -> step Down g
            KLeft       -> step Left g
            KRight      -> step Right  g
            --Input and remove numbers
            (KChar '1') -> insert (Lock 1 (focusedCell g)) g {previous = Just g}
            (KChar '2') -> insert (Lock 2 (focusedCell g)) g {previous = Just g}
            (KChar '3') -> insert (Lock 3 (focusedCell g)) g {previous = Just g}
            (KChar '4') -> insert (Lock 4 (focusedCell g)) g {previous = Just g}
            (KChar '5') -> insert (Lock 5 (focusedCell g)) g {previous = Just g}
            (KChar '6') -> insert (Lock 6 (focusedCell g)) g {previous = Just g}
            (KChar '7') -> insert (Lock 7 (focusedCell g)) g {previous = Just g}
            (KChar '8') -> insert (Lock 8 (focusedCell g)) g {previous = Just g}
            (KChar '9') -> insert (Lock 9 (focusedCell g)) g {previous = Just g}
            (KChar 'u') -> undo g
            KDel        -> deleteLocked (focusedCell g) g {previous = Just g}
            KBS         -> deleteLocked (focusedCell g) g {previous = Just g}
            _           -> g
--Everything else
handleEventEditor g _ =
    continue g

handleEventMenu :: Dialog Int -> BrickEvent Name a -> EventM Name (Next (Dialog Int))
--Navigate and pick option
handleEventMenu d (VtyEvent (EvKey key [])) =
    if key == KEnter then halt d else continue =<< handleDialogEvent (EvKey key []) d
--Everything else
handleEventMenu d _ =
    continue d

handleEventFileBrowser :: FileBrowser Name -> BrickEvent Name a -> EventM Name (Next (FileBrowser Name))
handleEventFileBrowser fb (VtyEvent (EvKey key [])) =
    case key of
        KUp         -> continue =<< actionFileBrowserListPrev fb
        KDown       -> continue =<< actionFileBrowserListNext fb
        KEnter      -> halt =<< actionFileBrowserSelectEnter fb
        (KChar 'q') -> halt fb
        _           -> continue fb
--Everything else
handleEventFileBrowser fb _ =
    continue fb

--DRAWING FUNCTIONS
--Composite of all widgets in game
drawGame :: Game -> [Widget Name]
drawGame g = 
    [center $ padRight (Pad 2) (drawGrid g) <+> (drawHelp <=> drawStatus g)]

drawEditor :: Game -> [Widget Name]
drawEditor g = 
    [center $ padRight (Pad 2) (drawGrid g) <+> drawHelpEditor]

{- highlightCursor cell g
Changes the background color of a cell if it's at the current cursor position
    RETURNS:    A widget with an attribute applied according to the type of the value in cell. If the coordinate of a cell == focusedCell g then color the background grey, 
                else use the default attribute for said cell value.
-}
hightlightCursor :: Cell -> Game -> Widget Name
hightlightCursor cell game =
    let coord = getCoordFromCell cell in
        if coord == focusedCell game then
            (\attr -> forceAttr attr (drawCell cell game))
            (case cell of
                (Input _ coord) | not (legalInput cell game) -> focusedIllegalInputAttr
                                | otherwise                  -> focusedInputAttr
                (Lock _ coord)  | not (legalInput cell game) -> focusedIllegalLockAttr
                                | otherwise                  -> focusedAttr
                (Note _ coord)  -> focusedNoteAttr
                (Empty coord)   -> focusedAttr)
        else (\attr -> withAttr attr (drawCell cell game))
            (case cell of
                (Input _ coord) -> inputAttr
                (Lock _ coord)  -> lockAttr
                (Note _ coord)  -> noteAttr
                (Empty coord)   -> defaultAttr)

{- drawCell cell g
Creates a widget from a cell value and game state. Colours the background red if the cell is illegal. 
    RETURNS:    a widget containing a cell value 'cell' from the current game state 'g'
    EXAMPLES:   -
-}
drawCell :: Cell -> Game -> Widget Name
drawCell cell game =
    let x = getIntFromCell cell in
    let filledCell = drawBigNumber x in
    case cell of
        (Lock x coord)  ->  if not (legalInput cell game) then
                                forceAttr illegalLockAttr filledCell
                            else filledCell
        (Input x coord) ->  if not (legalInput cell game) then
                                forceAttr illegalAttr filledCell
                            else filledCell
        (Note xs coord) ->
            let f x = if x `elem` xs then show x  ++ " " else "  "  in
            let xs' = map f [1..9] in
            withAttr noteAttr
            $ vBox
            $ map (hBox . (map str . (" " :))) (chunksOf 3 xs')
        (Empty coord)   -> str "       " <=> str "       " <=> str "       "

drawBigNumber :: Int -> Widget Name
drawBigNumber i = 
    case i of 
        1   -> vBox [str "  ╔╗   ", 
                     str "   ║   ", 
                     str "  ═╩═  "]
        2   -> vBox [str "  ╔═╗  ", 
                     str "  ╔═╝  ",
                     str "  ╚══  "]
        3   -> vBox [str "  ══╗  ", 
                     str "  ══╣  ",
                     str "  ══╝  "]
        4   -> vBox [str "  ╥ ╥  ", 
                     str "  ╚═╣  ",
                     str "    ╨  "]
        5   -> vBox [str "  ╔══  ", 
                     str "  ╚═╗  ",
                     str "  ══╝  "]
        6   -> vBox [str "  ╔══  ", 
                     str "  ╠═╗  ",
                     str "  ╚═╝  "]
        7   -> vBox [str "  ══╗  ", 
                     str "    ║  ",
                     str "    ╨  "]
        8   -> vBox [str "  ╔═╗  ", 
                     str "  ╠═╣  ",
                     str "  ╚═╝  "]
        9   -> vBox [str "  ╔═╗  ", 
                     str "  ╚═╣  ",
                     str "    ╨  "]
        _   -> str "       " <=> str "       " <=> str "       "
               

{- drawBox n g
Makes a Table widget from the cells in box n of game state
    RETURNS: a Table widget from the cells in box n in g.
    EXAMPLES: -
-}
drawBox :: Int -> Game -> Widget Name
drawBox n g =
    withBorderStyle unicode
    $ renderTable
    $ surroundingBorder False
    $ table
    $ chunksOf 3
    $ map (`hightlightCursor` g)
    $ toList
    $ box n g

{- drawGrid g
Composites all 9 boxes of game state into a grid
    RETURNS: A widget containing a grid which consists of all 9 boxes of g.
    EXAMPLES: -
-}
drawGrid :: Game -> Widget Name
drawGrid g =
    withBorderStyle unicodeBold
        $ joinBorders
        $ upperBorder
        <=>  vBox [
          hBox [outerVBorder, drawBox 1 g,  innerVBorder, drawBox 2 g, innerVBorder, drawBox 3 g, outerVBorder]
        , innerHBorder
        , hBox [outerVBorder, drawBox 4 g, innerVBorder, drawBox 5 g, innerVBorder,  drawBox 6 g, outerVBorder]
        , innerHBorder
        , hBox [outerVBorder, drawBox 7 g, innerVBorder, drawBox 8 g, innerVBorder, drawBox 9 g, outerVBorder]]
        <=> lowerBorder
        where
    innerVBorder    = setAvailableSize (1, 11) $ withBorderStyle unicodeBold vBorder
    innerHBorder    = setAvailableSize (73, 1) $ withBorderStyle unicodeBold (hBorderWithLabel (str "┣━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━╋━━━━━━━┿━━━━━━━┿━━━━━━━┫"))
    upperBorder     = setAvailableSize (73, 1) $ hBorderWithLabel (str "┏━━━━━━━┯━━━━━━━┯━━━━━━━┳━━━━━━━┯━━━━━━━┯━━━━━━━┳━━━━━━━┯━━━━━━━┯━━━━━━━┓")
    lowerBorder     = setAvailableSize (73, 1) $ hBorderWithLabel (str "┗━━━━━━━┷━━━━━━━┷━━━━━━━┻━━━━━━━┷━━━━━━━┷━━━━━━━┻━━━━━━━┷━━━━━━━┷━━━━━━━┛")
    outerVBorder    = setAvailableSize (1, 11) vBorder

--Info widget
--Defines a widget with instructions for how to play the game.
drawHelp :: Widget Name
drawHelp =
    withBorderStyle unicodeRounded
    $ borderWithLabel (str "Help")
    $ vLimitPercent 50
    $ padAll 1
    $ str "Navigate:       ↑ ↓ ← →" <=> 
      str "Exit:           Q" <=> 
      str "Insert number:  1-9" <=> 
      str "Toggle note:    Shift + 1-9" <=> 
      str "Undo:           U"<=> 
      str "Remove number:  Del/Backspace"


drawHelpEditor :: Widget Name
drawHelpEditor =
    withBorderStyle unicodeRounded
    $ borderWithLabel (str "Help")
    $ vLimitPercent 50
    $ str "Navigate: ↑ ↓ ← →" <=> str "Exit: Q" <=> str "Insert number: 1-9" <=> str "Remove number: Del/Backspace"

{- drawStatus g
Creates a widget that displays the current status of the game depending on
whether the grid is full, and if so if the solution is correct.  
    RETURNS:    A widget containing the current status of the game. 
    EXAMPLES:   -
-}
drawStatus:: Game -> Widget ()
drawStatus g
    | complete (isCompleted g) =
         withBorderStyle unicodeRounded
        $ borderWithLabel (str "Status")
        $ setAvailableSize (30, 5)
        $ padAll 1
        $ withAttr completeAttr 
        $ center
        $ str "CORRECT!"
    | isFull g =
         withBorderStyle unicodeRounded
        $ borderWithLabel (str "Status")
        $ setAvailableSize (30, 5)
        $ padAll 1
        $ withAttr incorrectAttr
        $ center
        $ str "Incorrect. Keep trying"
    | otherwise =
        withBorderStyle unicodeRounded
        $ borderWithLabel (str "Status")
        $ setAvailableSize (30, 5)
        $ padAll 1
        $ withAttr incompleteAttr
        $ center
        $ str "Incomplete"

{- drawMenu dialog
Renders the main menu
    RETURNS:    a list of the widgets rendered on the main menu, along with the dialog box options dialog
-}
drawMenu :: Dialog Int -> [Widget Name]
drawMenu d =
    [renderDialog d (center $ withAttr logoAttr haskudokuLogo) <+> padLeft (Pad 5) (hLimitPercent 12 (hCenter  $ strWrap "Created by Arvid Morelid, Ida Hellqvist and \nSimon Pislar"))]

drawSaveMenu :: Dialog Int -> [Widget Name]
drawSaveMenu d =
    [renderDialog d (center $ str "Do you want to save the game?")]

--Defines the options on the main menu
menuDialog :: Dialog Int
menuDialog =
    dialog Nothing (Just (0, [("Play", 0), ("Editor", 1), ("Help", 2), ("Quit", 3)])) 100

saveDialog :: Dialog Int
saveDialog =
    dialog Nothing (Just (1, [("Yes", 0), ("No", 1)])) 100

--Retrieves the number corresponding to the choice in a dialog
getChoice :: Dialog Int -> Maybe Int
getChoice =
    dialogSelection

--Renders the haskudoku logo
haskudokuLogo :: Widget Name
haskudokuLogo =
    vBox [
    str " _   _           _              _       _          "
  , str "| | | |         | |            | |     | |         "
  , str "| |_| | __ _ ___| | ___   _  __| | ___ | | ___   _ "
  , str "|  _  |/ _` / __| |/ / | | |/ _` |/ _ \\| |/ / | | |"
  , str "| | | | (_| \\__ \\   <| |_| | (_| | (_) |   <| |_| |"
  , str "\\_| |_/\\__,_|___/_|\\_\\\\__,_|\\__,_|\\___/|_|\\_\\\\__,_|"
    ]

--Renders the file browser along with a help section
drawFileBrowser :: FileBrowser Name -> [Widget Name]
drawFileBrowser fb =
    [renderFileBrowser True fb <=> withBorderStyle unicodeRounded (vBox [
                                                                    str "━━━━━━━━━━━━━━━━━━━━━━━━━━━",
                                                                    str "Help",
                                                                    str "━━━━━━━━━━━━━━━━━━━━━━━━━━━",
                                                                    str "Select file: Enter",
                                                                    str "Navigate:    Up/Down arrows",
                                                                    str "Go back:     Q"])]

--Used with setFileBrowserEntryFilter to filter out directories from the file browser
fileTypeFilter :: Maybe (FileInfo -> Bool)
fileTypeFilter =
    Just (fileTypeMatch [RegularFile])

--Retrieves the contents of the "Puzzles"-directory
fileBrowser :: IO (FileBrowser Name)
fileBrowser =
    newFileBrowser selectNonDirectories () (Just "Puzzles")

{-  emptyGame
    Generates an empty game. 
    RETURNS: A game with an empty grid and a focused cell at coordinates (5,5).
    EXAMPLES:
-}
emptyGame :: Game
emptyGame = Game {
    grid = newSudokuMatrix,
    focusedCell = (5, 5),
    complete = False,
    previous = Nothing
}

