{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module UI where
import Dat

import Graphics.Vty
import qualified Graphics.Vty

import Brick
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Brick as Brick.Types

import Data.Matrix
import Data.List.Split (chunksOf)
import Data.Char (digitToInt)

mkGame :: Game
mkGame = Game {
    grid = newSudokuMatrix ,
    focusedCell = (5, 5),
    complete = False
}

generalAttr = attrName "general"
attributes = attrMap defAttr [(generalAttr, bg blue)]

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

handleEvent :: Game -> BrickEvent Name a-> EventM Name (Next Game)
--Navigation
handleEvent g (VtyEvent (EvKey key [])) =
    case key of 
    --Navigation
    KUp         -> continue $ step Dat.Up g
    KDown       -> continue $ step Dat.Down g
    KLeft       -> continue $ step Dat.Left  g
    KRight      -> continue $ step Dat.Right  g
    --Global events
    (KChar 'q') -> halt g
    _           -> continue g
    --Input numbers
{- UNCOMMENT WHEN INPUT FUNCTION IS :: Int -> Coord -> Game -> Game
    (KChar '1') -> continue $ insert (Dat.Input 1) (focusedCell g) (grid g)
    (KChar '2') -> continue $ insert (Dat.Input 2) (focusedCell g) (grid g)
    (KChar '3') -> continue $ insert (Dat.Input 3) (focusedCell g) (grid g)
    (KChar '4') -> continue $ insert (Dat.Input 4) (focusedCell g) (grid g)
    (KChar '5') -> continue $ insert (Dat.Input 5) (focusedCell g) (grid g)
    (KChar '6') -> continue $ insert (Dat.Input 6) (focusedCell g) (grid g)
    (KChar '7') -> continue $ insert (Dat.Input 7) (focusedCell g) (grid g)
    (KChar '8') -> continue $ insert (Dat.Input 8) (focusedCell g) (grid g)
    (KChar '9') -> continue $ insert (Dat.Input 9) (focusedCell g) (grid g)
-}
--Resize
handleEvent g (VtyEvent (EvResize _ _ ))            = continue g

--DRAWING FUNCTIONS
--Composite of all widgets
drawGame :: Game -> [Widget Name]
drawGame g = 
     [center $ padRight (Pad 2) (vBox (map hBox (drawGrid g))) <+> (drawDebug g <=> drawHelp)]

--Cell widget
drawCell :: Cell -> Widget Name
drawCell cell = withBorderStyle unicode 
    $ border 
    $ hLimitPercent 10  
    $ vLimitPercent 10 
    $ case cell of
    Lock x      ->  str $ show x
    Dat.Input x ->  str $ show x
    Empty       ->  str " E "

--List all cell widgets in chunks of 9
drawGrid :: Game -> [[Widget Name]]
drawGrid g = 
    chunksOf 9 $ map drawCell (toList (grid g))

--Debug widget
drawDebug :: Game  -> Widget Name
drawDebug g = withBorderStyle unicodeBold 
    $ borderWithLabel (str "Debug")
    $ vLimitPercent 50 
    $ padAll 1
    $ str $ "Cursor pos: " ++ show (focusedCell g)

--Info widget
drawHelp :: Widget Name
drawHelp = withBorderStyle unicodeBold 
    $ borderWithLabel (str "Help")
    $ vLimitPercent 50 
    $ str "Navigate: \n ↑ ↓ ← →" <=> str "Exit: q"