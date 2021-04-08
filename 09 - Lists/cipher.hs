module Cipher where
import           Data.Char

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

caesar :: Int -> String -> String
caesar i = map (shift i)

unCaesar :: Int -> String -> String
unCaesar i = caesar (-i)
