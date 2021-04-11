module Vigenere where
import           Data.Char
import           Data.List

-- Reusage of the Caesar cipher helper functions
base :: Int
base = ord 'a'

alphabetIndex :: Char -> Int
alphabetIndex c = ord c - base

fromIndex :: Int -> Char
fromIndex = chr . (+ base)

shift :: Int -> Char -> Char
shift offset c = fromIndex (charIndex' `mod` 26)
 where
  charIndex  = offset + alphabetIndex c
  charIndex' = if charIndex < 0 then charIndex + 26 else charIndex

-- p. 447
type Keyword = String

mapToOffsetCycle :: Keyword -> [Int]
mapToOffsetCycle = concat . repeat . map alphabetIndex

shiftByOffsetCycle :: [Int] -> [String] -> [String]
shiftByOffsetCycle offsetCycle = reverse . foldl f []
 where
  f xs s = (zipWith shift shiftedOffsetCycle s) : xs
    where shiftedOffsetCycle = drop (length $ concat xs) offsetCycle

vigenere :: Keyword -> String -> String
vigenere key xs = intercalate " " $ shiftByOffsetCycle offsetCycle (words xs)
  where offsetCycle = mapToOffsetCycle key

-- For every keyword there is a negated keyword that deciphers the
-- Vigenere cipher. For instance, the negated keyword for "ally" is
-- "appc".
unvigenere :: Keyword -> String -> String
unvigenere key = vigenere negatedKey
  where negatedKey = map (fromIndex . (`mod` 26) . (26 -) . alphabetIndex) key
