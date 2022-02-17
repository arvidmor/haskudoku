module Main where

import Dat
import UI
import Brick (defaultMain)



main :: IO Game 
main = defaultMain app mkGame  