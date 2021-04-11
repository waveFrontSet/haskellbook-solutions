module AsPatterns where

import           Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _  = True
isSubseqOf _  [] = False
isSubseqOf xs@(x : rxs) (y : ys) | x == y    = isSubseqOf rxs ys
                                 | otherwise = isSubseqOf xs ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map f . words
 where
  f ""           = ("", "")
  f xs@(x : rxs) = (xs, (toUpper x) : rxs)
