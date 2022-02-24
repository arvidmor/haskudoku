{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

import Brick.Widgets.Table
    ( renderTable, surroundingBorder, table )
import Brick.Widgets.List (list, renderList)
import Data.List (intersperse, intercalate)

emptyGame :: Game
emptyGame = Game {
    grid = newSudokuMatrix,
    focusedCell = (5, 5),
    complete = False
}

mkGame :: Game
mkGame = insert (Input 6 (1,2)) (1, 2) $ insert (Lock 5 (1,1)) (1,1) Game {
    grid = newSudokuMatrix,
    focusedCell = (5, 5),
    complete = False
}

lockAttr    = attrName "Lock"
inputAttr   = attrName "Input"
noteAttr    = attrName "Note"
focusedAttr = attrName "Focused"
illegalAttr = attrName "Illegal"
focusedInputAttr = attrName "FocusedInput"
focusedNoteAttr  = attrName "FocusedNote"
defaultAttr      = attrName "Default"
focusedIllegalAttr = attrName "FocusedIllegal"
attributes = attrMap defAttr [
      (lockAttr, fg white)
    , (defaultAttr, defAttr)
    , (inputAttr, fg brightBlue )
    , (noteAttr, fg brightGreen)
    , (focusedAttr, bg brightBlack)
    , (illegalAttr, brightBlue `on` red)
    , (focusedInputAttr, brightBlue `on` brightBlack)
    , (focusedNoteAttr, brightGreen `on` brightBlack)
    , (focusedIllegalAttr, brightBlue `on` magenta)
    ]

menuAttributes = attrMap defAttr [
    (buttonSelectedAttr, bg brightBlack)
    , (buttonAttr , fg white)
    ]   

menuApp :: App (Dialog Int) a Name
menuApp = App {
    appDraw         = drawMenu
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEventMenu
  , appStartEvent   = return 
  , appAttrMap      = const menuAttributes
}

editorApp :: App Game a Name
editorApp = App {
    appDraw         = drawGame
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEventEditor
  , appStartEvent   = return
  , appAttrMap      = const attributes
}

app :: App Game a Name
app = App {
    appDraw         = drawGame
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const attributes
}

--EVENT HANDLING
{-
    Event handling inspired by Evan Relf: https://github.com/evanrelf/sudoku-tui.git
-}

handleEvent :: Game -> BrickEvent Name a -> EventM Name (Next Game)
--Navigation
handleEvent g (VtyEvent (EvKey key [])) =
    case key of
    --Navigation
    KUp         -> continue $ step Up g
    KDown       -> continue $ step Down g
    KLeft       -> continue $ step Left g
    KRight      -> continue $ step Right g
    --Input and remove numbers
    (KChar '1') -> continue $ insert (Input 1 (focusedCell g)) (focusedCell g) g
    (KChar '2') -> continue $ insert (Input 2 (focusedCell g)) (focusedCell g) g
    (KChar '3') -> continue $ insert (Input 3 (focusedCell g)) (focusedCell g) g
    (KChar '4') -> continue $ insert (Input 4 (focusedCell g)) (focusedCell g) g
    (KChar '5') -> continue $ insert (Input 5 (focusedCell g)) (focusedCell g) g
    (KChar '6') -> continue $ insert (Input 6 (focusedCell g)) (focusedCell g) g
    (KChar '7') -> continue $ insert (Input 7 (focusedCell g)) (focusedCell g) g
    (KChar '8') -> continue $ insert (Input 8 (focusedCell g)) (focusedCell g) g
    (KChar '9') -> continue $ insert (Input 9 (focusedCell g)) (focusedCell g) g
    KDel        -> continue $ delete (focusedCell g) g
    KBS         -> continue $ delete (focusedCell g) g
    -- Input number as note
    (KChar '!') -> continue $ insert (Note [1] (focusedCell g)) (focusedCell g) g
    (KChar '"') -> continue $ insert (Note [2] (focusedCell g)) (focusedCell g) g
    (KChar '#') -> continue $ insert (Note [3] (focusedCell g)) (focusedCell g) g
    (KChar '¤') -> continue $ insert (Note [4] (focusedCell g)) (focusedCell g) g
    (KChar '%') -> continue $ insert (Note [5] (focusedCell g)) (focusedCell g) g
    (KChar '&') -> continue $ insert (Note [6] (focusedCell g)) (focusedCell g) g
    (KChar '/') -> continue $ insert (Note [7] (focusedCell g)) (focusedCell g) g
    (KChar '(') -> continue $ insert (Note [8] (focusedCell g)) (focusedCell g) g
    (KChar ')') -> continue $ insert (Note [9] (focusedCell g)) (focusedCell g) g
     --Global events
    (KChar 'q') -> halt g
    _           -> continue g
--Resize
handleEvent g (VtyEvent (EvResize _ _ )) = continue g

handleEventEditor :: Game -> BrickEvent Name a -> EventM Name (Next Game)
handleEventEditor g (VtyEvent (EvKey key [])) =
    case key of
    --Navigation
    KUp         -> continue $ step Up g
    KDown       -> continue $ step Down g
    KLeft       -> continue $ step Left g
    KRight      -> continue $ step Right  g
    --Input and remove numbers
    (KChar '1') -> continue $ insert (Lock 1 (focusedCell g)) (focusedCell g) g
    (KChar '2') -> continue $ insert (Lock 2 (focusedCell g)) (focusedCell g) g
    (KChar '3') -> continue $ insert (Lock 3 (focusedCell g)) (focusedCell g) g
    (KChar '4') -> continue $ insert (Lock 4 (focusedCell g)) (focusedCell g) g
    (KChar '5') -> continue $ insert (Lock 5 (focusedCell g)) (focusedCell g) g
    (KChar '6') -> continue $ insert (Lock 6 (focusedCell g)) (focusedCell g) g
    (KChar '7') -> continue $ insert (Lock 7 (focusedCell g)) (focusedCell g) g
    (KChar '8') -> continue $ insert (Lock 8 (focusedCell g)) (focusedCell g) g
    (KChar '9') -> continue $ insert (Lock 9 (focusedCell g)) (focusedCell g) g
    KDel        -> continue $ deleteLocked (focusedCell g) g
    KBS         -> continue $ deleteLocked (focusedCell g) g
     --Global events
    (KChar 'q') -> halt g
    _           -> continue g
    --Resize
handleEventEditor g (VtyEvent (EvResize _ _ )) = continue g

handleEventMenu :: Dialog Int -> BrickEvent Name a -> EventM Name (Next (Dialog Int))
handleEventMenu d (VtyEvent (EvKey key [])) = 
    --Navigate and pick option
    if key == KEnter then halt d else continue =<< handleDialogEvent (EvKey key []) d
--Resize
handleEventMenu d (VtyEvent (EvResize _ _))    = continue d

--DRAWING FUNCTIONS
--Composite of all widgets
drawGame :: Game -> [Widget Name]
drawGame g =
    [center $ padRight (Pad 2) (drawGrid g) <+> (drawDebug g <=> drawHelp)]


legalInput :: Cell -> Game -> Bool
legalInput cell game = legalInSubGrid cell (listSubGrid (getCoordFromCell cell)) game && legalInRow cell game && legalInCol cell game


hightlightCursor :: Cell -> Game -> Widget Name
hightlightCursor cell game = let coord = getCoordFromCell cell in
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



--Cell widget. Draws focused cell with a "lightBlack" background color
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
            $ map hBox
            $ chunksOf 3 
            $ map str xs'
        (Empty coord)   -> str "       " <=> str "       " <=> str "       "

--Makes a Table widget from the cells in box n of game state
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

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle unicodeBold
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

--Debug widget
drawDebug :: Game  -> Widget Name
drawDebug g = withBorderStyle unicodeRounded
    $ borderWithLabel (str "Debug")
    $ vLimitPercent 50
    $ padAll 1
    $ str $ "Cursor position: " ++ show (focusedCell g)

--Info widget
drawHelp :: Widget Name
drawHelp = withBorderStyle unicodeRounded
    $ borderWithLabel (str "Help")
    $ vLimitPercent 50
    $ str "Navigate: \n ↑ ↓ ← →" <=> str "Exit: q" <=> str "Insert number: 1-9" <=> str "Insert note: Shift + 1-9"<=> str "Remove number: Del/Backspace" 

drawMenu :: Dialog Int -> [Widget Name]
drawMenu d = [renderDialog d (center haskudokuLogo)]

haskudokuLogo :: Widget Name
haskudokuLogo = vBox [
    str " _   _           _              _       _          "
  , str "| | | |         | |            | |     | |         "
  , str "| |_| | __ _ ___| | ___   _  __| | ___ | | ___   _ "
  , str "|  _  |/ _` / __| |/ / | | |/ _` |/ _ \\| |/ / | | |"
  , str "| | | | (_| \\__ \\   <| |_| | (_| | (_) |   <| |_| |"
  , str "\\_| |_/\\__,_|___/_|\\_\\\\__,_|\\__,_|\\___/|_|\\_\\\\__,_|"
    ]

menuDialog :: Dialog Int
menuDialog = dialog Nothing (Just (0, [("Load", 0), ("Editor", 1), ("Help", 2), ("Quit", 3)])) 100

getChoice :: Dialog Int -> Maybe Int
getChoice = dialogSelection 

