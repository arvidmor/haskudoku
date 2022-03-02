module UI where
import Solver
import Types
import Grid

import Graphics.Vty hiding (Input)
import qualified Graphics.Vty

import Brick hiding (Up, Down)
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Dialog
import Brick.Widgets.Border.Style
import qualified Brick as Brick.Types

import Data.Matrix
import Data.List.Split (chunksOf)
import Prelude hiding (Right, Left)

import Brick.Widgets.Table ( renderTable, surroundingBorder, table )
import Brick.Widgets.List (list, renderList, listSelectedAttr)
import Data.List (intersperse, intercalate)
import Brick.Widgets.FileBrowser

-- ATTRIBUTES
lockAttr, inputAttr, noteAttr, focusedAttr, illegalAttr, focusedInputAttr, focusedNoteAttr, focusedIllegalAttr, defaultAttr, logoAttr :: AttrName
lockAttr            = attrName "Lock"
inputAttr           = attrName "Input"
noteAttr            = attrName "Note"
focusedAttr         = attrName "Focused"
illegalAttr         = attrName "Illegal"
focusedInputAttr    = attrName "FocusedInput"
focusedNoteAttr     = attrName "FocusedNote"
focusedIllegalAttr  = attrName "FocusedIllegal"
defaultAttr         = attrName "Default"
logoAttr            = attrName "Logo"

-- ATTRIBUTE MAPS
gameAttrs, menuAttrs, fileBrowserAttrs :: AttrMap
gameAttrs = attrMap defAttr [
    (lockAttr, fg white),
    (defaultAttr, defAttr),
    (inputAttr, fg brightBlue),
    (noteAttr, fg brightGreen),
    (focusedAttr, bg brightBlack),
    (illegalAttr, brightBlue `on` red),
    (focusedInputAttr, brightBlue `on` brightBlack),
    (focusedNoteAttr, brightGreen `on` brightBlack),
    (focusedIllegalAttr, brightBlue `on` magenta)
    ]
menuAttrs = attrMap defAttr [
    (buttonSelectedAttr, bg brightBlack),
    (buttonAttr, fg white),
    (logoAttr, fg green)
    ]
fileBrowserAttrs = attrMap defAttr [
    (fileBrowserRegularFileAttr, fg green),
    (listSelectedAttr, green `on` brightBlack)
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
    appDraw         = drawGame,
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
            (KChar '1') -> insert (Input 1 coord) g
            (KChar '2') -> insert (Input 2 coord) g
            (KChar '3') -> insert (Input 3 coord) g
            (KChar '4') -> insert (Input 4 coord) g
            (KChar '5') -> insert (Input 5 coord) g
            (KChar '6') -> insert (Input 6 coord) g
            (KChar '7') -> insert (Input 7 coord) g
            (KChar '8') -> insert (Input 8 coord) g
            (KChar '9') -> insert (Input 9 coord) g
            KDel        -> delete coord g
            KBS         -> delete coord g
            --Toggle notes
            (KChar '!') -> toggleNote 1 coord g
            (KChar '"') -> toggleNote 2 coord g
            (KChar '#') -> toggleNote 3 coord g
            (KChar '¤') -> toggleNote 4 coord g
            (KChar '%') -> toggleNote 5 coord g
            (KChar '&') -> toggleNote 6 coord g
            (KChar '/') -> toggleNote 7 coord g
            (KChar '(') -> toggleNote 8 coord g
            (KChar ')') -> toggleNote 9 coord g
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
            (KChar '1') -> insert (Lock 1 (focusedCell g)) g
            (KChar '2') -> insert (Lock 2 (focusedCell g)) g
            (KChar '3') -> insert (Lock 3 (focusedCell g)) g
            (KChar '4') -> insert (Lock 4 (focusedCell g)) g
            (KChar '5') -> insert (Lock 5 (focusedCell g)) g
            (KChar '6') -> insert (Lock 6 (focusedCell g)) g
            (KChar '7') -> insert (Lock 7 (focusedCell g)) g
            (KChar '8') -> insert (Lock 8 (focusedCell g)) g
            (KChar '9') -> insert (Lock 9 (focusedCell g)) g
            KDel        -> deleteLocked (focusedCell g) g
            KBS         -> deleteLocked (focusedCell g) g
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
                (Input _ coord) -> focusedInputAttr
                (Lock _ coord)  -> focusedAttr
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
    let filledCell = str "       " <=> str ("   " ++ show x ++ "   ") <=> str "       " in
    case cell of
        (Lock x coord)  ->
            filledCell
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
    $ str "Navigate: \n ↑ ↓ ← →" <=> str "Exit: Q" <=> str "Insert number: 1-9" <=> str "Insert note: Shift + 1-9"<=> str "Remove number: Del/Backspace"

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
        $ str "CORRECT! Well done!    "
    | isFull g =
         withBorderStyle unicodeRounded
        $ borderWithLabel (str "Status")
        $ setAvailableSize (30, 5)
        $ padAll 1
        $ str "Incorrect. Keep trying."
    | otherwise =
         withBorderStyle unicodeRounded
        $ borderWithLabel (str "Status")
        $ setAvailableSize (30, 5)
        $ padAll 1
        $ str "Incomplete             "

{- drawMenu dialog
Renders the main menu
    RETURNS:    a list of the widgets rendered on the main menu, along with the dialog box options dialog
-}
drawMenu :: Dialog Int -> [Widget Name]
drawMenu d =
    [renderDialog d (center $ withAttr logoAttr haskudokuLogo) <+> padLeft (Pad 5) (hLimitPercent 12 (hCenter  $ strWrap "Created by Arvid Morelid, Ida Hellqvist and \nSimon Pislar"))]

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

--Defines the options on the main menu
menuDialog :: Dialog Int
menuDialog =
    dialog Nothing (Just (0, [("Load", 0), ("Editor", 1), ("Help", 2), ("Quit", 3)])) 100

--Retrieves the number corresponding to the choice in a dialog
getChoice :: Dialog Int -> Maybe Int
getChoice =
    dialogSelection

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
    complete = False
}

