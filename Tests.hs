module Tests where

import FileIO
import Grid
import Main
import Solver
import Types
import UI
import Test.HUnit
import Test.QuickCheck

--Grid functions
--Main Solver functions
--Testcase to make sure that stepping R/L and U/D are inverse operations no matter where in the grid 
test1 = TestCase $ assertEqual "step" (emptyGame {focusedCell = (1, 1)}) (step Types.Down $ step Types.Up $ step Types.Left $ step Types.Right (emptyGame {focusedCell = (1, 1)}))

--Toggling a note on and off gives an empty cell again
test2 = TestCase $ assertEqual "toggleNote" (grid emptyGame) (grid (toggleNote 1 (5, 5) $ toggleNote 1 (5, 5) emptyGame))