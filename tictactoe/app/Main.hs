module Main where

import Lib (mainLoop, generateGameState)

main :: IO ()
main = mainLoop generateGameState False 0
