module Lib
    ( mainLoop
    ) where
import Text.Format
import Control.Lens
import GHC.Show (intToDigit)

-- [
--     [0,0,0],
--     [0,0,0],
--     [0,0,0]
-- ]

generateGameState :: [[Int]]
generateGameState = [[0,0,0] | i <- [1..3]]

getCharacter :: Int -> [Char]
getCharacter n
    | n == 1 = "X"
    | n == 2 = "O"
    | otherwise  = " "

getGameLine :: [Int] -> [Char]
getGameLine l = format "[ {0} | {1} | {2}]" [getCharacter (head l), getCharacter (l!!1), getCharacter (l!!2)]

printGameState :: [[Int]] -> IO ()
printGameState gameState = do 
    putStrLn $ getGameLine (head gameState)
    putStrLn $ getGameLine (gameState!!1)
    putStrLn $ getGameLine (gameState!!2)

playCell :: Int -> Int -> [[Int]] -> [[Int]]
playCell x y gameState = gameState & element y .~ ( gameState!!y & element x .~ 1)

mainLoop :: IO ()
mainLoop = do
    let gameState = generateGameState
    let gameOver = False
    if not gameOver then 
        do
            printGameState gameState
            putStrLn "Your turn"
            putStrLn "Enter row: "
            row <- getLine 
            let y = (read row :: Int)
            putStrLn "Enter column: "
            column <- getLine
            let x = (read column :: Int)
            let gameState = playCell x y gameState
            putStrLn "Turn finished"
            mainLoop
        else 
            putStrLn "Fin de la partie"