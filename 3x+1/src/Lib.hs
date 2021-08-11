module Lib
    (
      stepAction,
      startCycle
    ) where
        
import Control.Monad ( when )


-- the idea of this small program is this video:
-- https://youtu.be/094y1Z2wpJg by veritasium
-- I needed something to practice and this seemed interesting

-- 3x+1 or /2
stepAction :: Integer -> Integer
stepAction n = if even n then n `div`  2 else n * 3 + 1

-- recursive calls until 1
startCycle :: Integer -> [Integer] -> [Integer]
startCycle n list = do
    let step = stepAction n
    if n == 1 
        then list++[1] 
        else startCycle step (list++[n])