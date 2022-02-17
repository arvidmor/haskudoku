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
import Data.Char (digitToInt)
import Prelude hiding (Right, Left)

mkGame :: Game
mkGame = insert (Input 6) (1, 2) $ insert (Lock 5) (1,1) Game {
    grid = newSudokuMatrix ,
    focusedCell = (5, 5),
    complete = False
}

lockAttr    = attrName "Lock"
inputAttr   = attrName "Input"
attributes = attrMap defAttr [
      (lockAttr, white `on` black)
    , (inputAttr, blue `on` black)
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
    --Input numbers
    (KChar '1') -> continue $ insert (Input 1) (focusedCell g) g
    (KChar '2') -> continue $ insert (Input 2) (focusedCell g) g
    (KChar '3') -> continue $ insert (Input 3) (focusedCell g) g
    (KChar '4') -> continue $ insert (Input 4) (focusedCell g) g
    (KChar '5') -> continue $ insert (Input 5) (focusedCell g) g
    (KChar '6') -> continue $ insert (Input 6) (focusedCell g) g
    (KChar '7') -> continue $ insert (Input 7) (focusedCell g) g
    (KChar '8') -> continue $ insert (Input 8) (focusedCell g) g
    (KChar '9') -> continue $ insert (Input 9) (focusedCell g) g
     --Global events
    (KChar 'q') -> halt g
    _           -> continue g
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
    Lock x      ->  withAttr lockAttr  $ str $ show x
    Input x     ->  withAttr inputAttr $ str $ show x
    Empty       ->  str "  "

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

drawTest :: Game -> Widget Name
drawTest g = undefined --withBorderStyle unicodeRounded 