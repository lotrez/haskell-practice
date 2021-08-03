module Lib
    ( someFunc,
      stepAction,
      startCycle
    ) where
        
import Control.Monad ( when )

someFunc :: IO ()
someFunc = putStrLn "someFunc"
stepAction :: Int -> Int
stepAction n = if even n then n `div`  2 else n * 3 + 1


startCycle :: Int -> [Int] -> [Int]
startCycle n list = do
    let step = stepAction n
    if n == 1 
        then list++[1] 
        else startCycle step (list++[n])