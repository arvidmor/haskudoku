{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module UI where
import Dat

import Graphics.Vty hiding (Input)
import qualified Graphics.Vty

import Brick hiding (Up, Down)
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
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
focusedAttr = attrName "Focused"
focusedInputAttr = attrName "FocusedInput"
attributes = attrMap defAttr [
      (lockAttr, fg white)
    , (inputAttr, fg brightBlue )
    , (focusedAttr, bg brightBlack)
    , (focusedInputAttr, brightBlue `on` brightBlack)
    ]

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
    KRight      -> continue $ step Right  g
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

--DRAWING FUNCTIONS
--Composite of all widgets
drawGame :: Game -> [Widget Name]
drawGame g =
    [center $ padRight (Pad 2) (drawGrid g) <+> (drawDebug g <=> drawHelp)]

--Cell widget. Draws focused cell with a "lightBlack" background color
drawCell :: Cell -> Game -> Widget Name
drawCell cell game = 
    case cell of 
        (Lock x coord)  -> 
            if coord == focusedCell game then
                withAttr focusedAttr filledCell
            else
                withAttr lockAttr filledCell
            where
                filledCell = str "       " <=> str ("   " ++ show x ++ "   ") <=> str "       "

        (Input x coord) ->  
            if coord == focusedCell game then 
                withAttr focusedInputAttr filledCell
            else
                withAttr inputAttr filledCell
            where 
                filledCell = str "       " <=> str ("   " ++ show x ++ "   ") <=> str "       "

        (Empty coord)   ->
            if coord == focusedCell game then        
                withAttr focusedAttr emptyCell
            else
                emptyCell
            where 
                emptyCell  = str "       " <=> str "       " <=> str "       "

--Makes a Table widget from the cells in box n of game state
drawBox :: Int -> Game -> Widget Name
drawBox n g =
    withBorderStyle unicode 
    $ renderTable
    $ surroundingBorder False 
    $ table 
    $ chunksOf 3
    $ map (`drawCell` g)
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
    $ str $ "Cursor pos: " ++ show (focusedCell g)

--Info widget
drawHelp :: Widget Name
drawHelp = withBorderStyle unicodeRounded
    $ borderWithLabel (str "Help")
    $ vLimitPercent 50
    $ str "Navigate: \n ↑ ↓ ← →" <=> str "Exit: q" <=> str "Insert number: 1-9" <=> str "Remove number: Del/Backspace"
