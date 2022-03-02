module Tests where

import FileIO
import Grid
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

--Illegal to add a number to a subgrid where that number is already present 
test3 = TestCase $ assertEqual "legalSubgrid" False (legalInSubGrid (Input 1 (5,5)) (listSubGrid (getCoordFromCell (Input 1 (5,5)))) (insert (Input 1 (4,5)) emptyGame))

--Illegal to add a number to a row where that number is already present 
test4 = TestCase $ assertEqual "legalRow" False (legalInRow (Input 1 (5,5)) (insert (Input 1 (5,4)) emptyGame))

--Illegal to add a number to a column where that number is already present 
test5 = TestCase $ assertEqual "legalColumn" False (legalInCol (Input 1 (5,5)) (insert (Input 1 (4,5)) emptyGame))
tests = TestList [test1, test2, test3, test4, test5]

performTests = runTestTT tests