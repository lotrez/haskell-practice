import Control.Monad
import Lib ( 
        checkIfNumber,
        getFirstIndex
        )
import Data.Maybe (isJust)

main :: IO ()
main = do
        testIfNumber
        testFirstIndex

testFirstIndex :: IO ()
testFirstIndex = do
        putStrLn "Testing firstIndex"
        when (getFirstIndex "aze2" 0 /= 3) $ do
                print (getFirstIndex "aze2" 0)
                error "testFirstIndexError"
        when (getFirstIndex "azeazeeza" 0 /= -1) $ do
                print (getFirstIndex "azeazeeza" 0)
                error "testFirstIndexError"

testIfNumber :: IO ()
testIfNumber = do
        putStrLn "Testing checkNumber"
        unless (isJust (checkIfNumber '2')) $ do
                print (checkIfNumber '2')
                error "testNumberError"
        unless (isJust (checkIfNumber '8')) $ do
                print (checkIfNumber '8')
                error "testNumberError"
        unless (isJust (checkIfNumber '4')) $ do
                print (checkIfNumber '4')
                error "testNumberError"
        when (isJust (checkIfNumber 'a')) $ do
                print (checkIfNumber 'a')
                error "testNumberError"
        when (isJust (checkIfNumber 'x')) $ do
                print (checkIfNumber 'x')
                error "testNumberError"
        when (isJust (checkIfNumber '^')) $ do
                print (checkIfNumber '^')
                error "testNumberError"

