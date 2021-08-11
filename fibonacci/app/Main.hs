module Main where
import Lib ( fibonacci )

import System.Environment ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    let x = (read (head args) :: Integer)
    print $ fibonacci x
