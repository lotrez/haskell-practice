module Main  where
import Lib ( startCycle )



main :: IO ()
main = 
    do
        putStrLn "Enter your number"
        line <- getLine 
        let n = read line :: Integer
        
        print(startCycle n [])
