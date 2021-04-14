-- Chapter exercise 1
module CharFuncs where

import           Data.Char

filterUpper :: String -> String
filterUpper = filter isUpper

capFirstLetter :: String -> String
capFirstLetter ""       = ""
capFirstLetter (x : xs) = toUpper x : xs

capAllLetters :: String -> String
capAllLetters = map toUpper

getFirstLetter :: String -> Char
getFirstLetter = toUpper . head
