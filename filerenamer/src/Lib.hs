module Lib
    (
        -- parseFileName,
        getFirstIndex,
        checkIfNumber
    ) where
import Control.Exception.Base (typeError, try, evaluate, SomeException (SomeException))
import Text.Read (readMaybe)
import Data.Maybe (isJust)
checkIfNumber :: Char -> Maybe Int
checkIfNumber l = readMaybe [l] :: Maybe Int


-- get index of first number that has a letter preceding it
getFirstIndex :: [Char] -> Int -> Int
getFirstIndex fileName i =
    -- string is empty (no number in it)
    if  null fileName
        then -1
        else do
            -- try catch to see if it's a letter or number
            let isANumber = checkIfNumber (head fileName)
            -- ?
            if isJust isANumber
                then i
                else do
                    let newFileName = drop 1 fileName
                    getFirstIndex newFileName (i+1)

-- we need to get the number of the season then the number of the episode
-- parseFileName :: [Char] -> [Char] -> Int -> Int
-- parseFileName fileName = 