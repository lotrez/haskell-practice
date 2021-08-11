module Lib
    ( mainLoop, generateGameState
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
getGameLine l = format "[{0}|{1}|{2}]" [getCharacter (head l), getCharacter (l!!1), getCharacter (l!!2)]

printGameState :: [[Int]] -> IO ()
printGameState gameState = do
    putStrLn $ getGameLine (head gameState)
    putStrLn $ getGameLine (gameState!!1)
    putStrLn $ getGameLine (gameState!!2)

playCell :: Int -> Int -> [[Int]] -> Int -> [[Int]]
playCell x y gameState turnNumber =
    do
        let correctX = x-1
        let correctY = y-1
        let player = if even turnNumber then 1 else 2
        gameState & element correctY .~ ( gameState!!correctY & element correctX .~ player)

mainLoop :: [[Int]] -> Bool -> Int -> IO ()
mainLoop gameState gameOver turnNumber = do
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
            putStrLn "Turn finished"
            let newGameState = playCell x y gameState turnNumber
            if turnNumber == 2
                then
                    mainLoop newGameState True (turnNumber+1)
                else
                    mainLoop newGameState False (turnNumber+1)
        else
            putStrLn "Fin de la partie"