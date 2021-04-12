module ValidateWord where

import           Data.Char

newtype Word' = Word' String deriving (Eq, Show)


vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel x = (toLower x) `elem` vowels

mkWord :: String -> Maybe Word'
mkWord x | numberOfVowels > numberOfCons = Nothing
         | otherwise                     = Just (Word' x)
 where
  numberOfVowels = length $ filter isVowel x
  numberOfCons   = (length x) - numberOfVowels
