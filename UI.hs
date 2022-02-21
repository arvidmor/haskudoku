{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module UI where
import Dat
import qualified Dat

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
import Brick.Widgets.List (list, renderList)
import Data.List (intersperse, intercalate)

mkGame :: Game
mkGame = insert (Input 6) (1, 2) $ insert (Lock 5) (1,1) Game {
    grid = newSudokuMatrix,
    focusedCell = (5, 5),
    complete = False
}

lockAttr    = attrName "Lock"
inputAttr   = attrName "Input"
attributes = attrMap defAttr [
      (lockAttr, white `on` black)
    , (inputAttr, brightBlack   `on` black)
    ]

app :: App Game a Name
app = App { appDraw         = drawGame
          , appChooseCursor = showFirstCursor
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
    KLeft       -> continue $ step Left  g
    KRight      -> continue $ step Right  g
    --Input and remove numbers
    (KChar '1') -> continue $ insert (Input 1) (focusedCell g) g
    (KChar '2') -> continue $ insert (Input 2) (focusedCell g) g
    (KChar '3') -> continue $ insert (Input 3) (focusedCell g) g
    (KChar '4') -> continue $ insert (Input 4) (focusedCell g) g
    (KChar '5') -> continue $ insert (Input 5) (focusedCell g) g
    (KChar '6') -> continue $ insert (Input 6) (focusedCell g) g
    (KChar '7') -> continue $ insert (Input 7) (focusedCell g) g
    (KChar '8') -> continue $ insert (Input 8) (focusedCell g) g
    (KChar '9') -> continue $ insert (Input 9) (focusedCell g) g
    KDel        -> continue $ delete (focusedCell g) g
    KBS         -> continue $ delete (focusedCell g) g
     --Global events
    (KChar 'q') -> halt g
    _           -> continue g
--Resize
handleEvent g (VtyEvent (EvResize _ _ )) = continue g

--DRAWING FUNCTIONS
--Composite of all widgets
drawGame :: Game -> [Widget Name]
drawGame g =
     [center $ padRight (Pad 2) (drawGrid g) <+> (drawDebug g <=> drawHelp)]

--Cell widget
drawCell :: Cell -> Widget Name
drawCell cell = 
    case cell of
    Lock x      ->  withAttr lockAttr  $ str (" " ++ show x ++ " ")
    Input x     ->  withAttr inputAttr $ str (" " ++ show x ++ " ")
    Empty       ->  str "   "

--Makes a widget from the cells in box n of game state
drawBox :: Int -> Game -> Widget Name
drawBox n g =
    withBorderStyle unicode 
    $ renderTable
    $ surroundingBorder False 
    $ table 
    $ chunksOf 3
    $ map drawCell
    $ toList 
    $ box n g

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle unicodeBold 
        $ border
        $ joinBorders 
        $ vBox [
          hBox [drawBox 1 g, verticalBorder, drawBox 2 g, verticalBorder, drawBox 3 g]
        , horizontalBorder
        , hBox [drawBox 4 g, verticalBorder, drawBox 5 g, verticalBorder,  drawBox 6 g]
        , horizontalBorder
        , hBox [drawBox 7 g, verticalBorder, drawBox 8 g, verticalBorder, drawBox 9 g]
         ]
        where
    verticalBorder = setAvailableSize (1, 5) (withBorderStyle unicodeBold vBorder)
    horizontalBorder = setAvailableSize (35, 1) (withBorderStyle unicodeBold (hBorderWithLabel (str "━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━")))
    -- ━
    -- ┼
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

{- box n game
Creates a submatrix corresponding to the n'th box of the sudoku-grid
    RETURNS:    the n'th box of the grid in the current state game
    EXAMPLES:   
-}
box :: Int -> Game -> Matrix Cell
box n game = case n of 
    1   -> submatrix 1 3 1 3 (grid game)
    2   -> submatrix 1 3 4 6 (grid game)
    3   -> submatrix 1 3 7 9 (grid game)
    4   -> submatrix 4 6 1 3 (grid game)
    5   -> submatrix 4 6 4 6 (grid game)
    6   -> submatrix 4 6 7 9 (grid game)
    7   -> submatrix 7 9 1 3 (grid game)
    8   -> submatrix 7 9 4 6 (grid game)
    9   -> submatrix 7 9 7 9 (grid game)