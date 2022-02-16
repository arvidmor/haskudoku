{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module UI where
import Dat

import Graphics.Vty
import qualified Graphics.Vty

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

mkGame :: Game
mkGame = Game {
    grid = newSudokuMatrix ,
    focusedCell = (5, 5),
    complete = False
}

app :: App Game a Name
app = App { appDraw         = drawGame
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = undefined 
          }


handleEvent :: Game -> BrickEvent Name a-> EventM Name (Next Game)
handleEvent g (VtyEvent (EvKey KUp []))             = continue $ step Dat.Up g
handleEvent g (VtyEvent (EvKey KDown []))           = continue $ step Dat.Down g
handleEvent g (VtyEvent (EvKey KLeft []))           = continue $ step Dat.Left  g
handleEvent g (VtyEvent (EvKey KRight []))          = continue $ step Dat.Right  g

drawGame :: Game -> [Widget Name]
drawGame g = 
     [center $ padRight (Pad 2) (drawDebug g)]

drawDebug :: Game  -> Widget Name
drawDebug g = withBorderStyle unicodeBold 
    $ borderWithLabel (str "Debug")
    $ hCenter
    $ padAll 1
    $ str $ show $ focusedCell g
