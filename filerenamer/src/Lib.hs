module Lib
    (
        -- parseFileName,
        getFirstIndex,
        checkIfNumber
    ) where

-- check if char is a number
-- check if char is a number
-- check if char is a number
-- check if char is a number
-- check if char is a number
-- check if char is a number
-- check if char is a number
-- check if char is a number
import Control.Exception.Base (typeError, try, evaluate, SomeException (SomeException))
import Text.Read (readMaybe)
import Data.Maybe (isJust)
checkIfNumber :: Char -> Maybe Int
checkIfNumber l = readMaybe [l] :: Maybe Int


-- get index of first number that has a letter preceding it
getFirstIndex :: [Char] -> Int -> Int
getFirstIndex fileName i = do
    -- try catch to see if it's a letter or number
    let isANumber = checkIfNumber (head fileName)
    -- ?
    if isJust isANumber
        then i
        else do
            -- check if the char behind is a number
            if isJust (checkIfNumber (fileName !! 1))
                then  i+1
                -- if it isnt call again with head
                else do
                    let newFileName = drop 1 fileName
                    getFirstIndex newFileName (i+1)

-- parseFileName :: [Char] -> [Char] -> Int -> Int
-- parseFileName fileName = 