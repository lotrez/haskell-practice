import Control.Monad
import Lib ( checkIfNumber )
import Data.Maybe (isJust)

main :: IO ()
main = do
        testIfNumber
		testFirstIndex

testFirstIndex :: IO ()


testIfNumber :: IO ()
testIfNumber = do
        putStrLn "Testing the checkNumber"
        unless (isJust (checkIfNumber '2')) $ do
                print (checkIfNumber '2')
                error "cycleFunctionError"
        unless (isJust (checkIfNumber '8')) $ do
                print (checkIfNumber '8')
                error "cycleFunctionError"
        unless (isJust (checkIfNumber '4')) $ do
                print (checkIfNumber '4')
                error "cycleFunctionError"
        when (isJust (checkIfNumber 'a')) $ do
                print (checkIfNumber 'a')
                error "cycleFunctionError"
        when (isJust (checkIfNumber 'x')) $ do
                print (checkIfNumber 'x')
                error "cycleFunctionError"
        when (isJust (checkIfNumber '^')) $ do
                print (checkIfNumber '^')
                error "cycleFunctionError"

