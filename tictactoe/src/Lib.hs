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

getPlayerNumber :: Int -> Int
getPlayerNumber turnNumber = if even turnNumber then 1 else 2

playCell :: Int -> Int -> [[Int]] -> Int -> [[Int]]
playCell x y gameState turnNumber =
    do
        let correctX = x-1
        let correctY = y-1
        let player = getPlayerNumber turnNumber
        gameState & element correctY .~ ( gameState!!correctY & element correctX .~ player)

-- returns the number of the player that won
checkGameOver :: [[Int]] -> Int
checkGameOver board
    | checkIfPlayerWon board 1 = 1
    | checkIfPlayerWon board 2 = 2
    | otherwise = 0

checkIfPlayerWon :: [[Int]] -> Int -> Bool
checkIfPlayerWon board player = 
    -- check rows
    -- all (\col -> b!!row!!col == 0) [0..2] -> check if line is all player
    -- then for every rows
    any (\row -> all (\col -> board!!row!!col == player) [0..2]) [0..2] ||
    -- reversed for columns
    any (\row -> all (\col -> board!!col!!row == player) [0..2]) [0..2] ||
    -- check diagonals
    (board!!0!!0 == player && board!!1!!1 == player && board!!2!!2 == player) ||
    (board!!2!!0 == player && board!!1!!1 == player && board!!0!!2 == player)


mainLoop :: [[Int]] -> Bool -> Int -> IO ()
mainLoop gameState gameOver turnNumber = do
    if not gameOver then
        do
            printGameState gameState
            putStrLn (format "Player {0}'s turn" [[intToDigit (getPlayerNumber turnNumber)]])
            putStrLn "Enter row: "
            row <- getLine
            let y = (read row :: Int)
            putStrLn "Enter column: "
            column <- getLine
            let x = (read column :: Int)
            putStrLn "Turn finished"
            let newGameState = playCell x y gameState turnNumber
            -- isGameOver is an int
            let isGameOver = checkGameOver newGameState
            if isGameOver == 0
                then
                    mainLoop newGameState False (turnNumber+1)
                else
                    do
                        putStrLn (format "player {0} won" [[intToDigit isGameOver]])
                        printGameState newGameState
                    
        else
            putStrLn "Fin de la partie"