module ForLinguists where

import           Data.Char
import           Data.List

-- 1.
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

replaceThe :: String -> String
replaceThe = intercalate " " . map (replaceByA . notThe) . words
 where
  replaceByA Nothing  = "a"
  replaceByA (Just x) = x

-- 2.
vowels :: [Char]
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel x = (toLower x) `elem` vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = filterCounts 0 . words
 where
  filterCounts s []  = s
  filterCounts s [_] = s
  filterCounts s ("the" : x : xs) | isVowel $ head x = filterCounts (s + 1) xs
                                  | otherwise        = filterCounts s xs
  filterCounts s (_ : xs) = filterCounts s xs

-- 3.
filterVowels :: String -> String
filterVowels = filter isVowel

countVowels :: String -> Int
countVowels = length . filterVowels
