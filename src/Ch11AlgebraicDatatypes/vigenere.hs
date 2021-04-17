module Ch11AlgebraicDatatypes.Vigenere where
import           Data.Char

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
mapToOffsetCycle keyword
  | keyword == "" = repeat 0
  | otherwise     = concat $ repeat $ map alphabetIndex keyword

shiftByOffsetCycle :: [Int] -> String -> String
shiftByOffsetCycle offsetCycle = reverse . foldl f ""
 where
  f :: String -> Char -> String
  f xs c | c == ' '  = ' ' : xs
         | otherwise = shift index c : xs
    where index = head $ drop (length $ filter (/= ' ') xs) offsetCycle

vigenere :: Keyword -> String -> String
vigenere key xs = shiftByOffsetCycle offsetCycle xs
  where offsetCycle = mapToOffsetCycle key

-- For every keyword there is a negated keyword that deciphers the
-- Vigenere cipher. For instance, the negated keyword for "ally" is
-- "appc".
unvigenere :: Keyword -> String -> String
unvigenere key = vigenere negatedKey
  where negatedKey = map (fromIndex . (`mod` 26) . (26 -) . alphabetIndex) key

-- Chapter 13
main :: IO ()
main = do
  putStrLn "Enter keyword:"
  keyword <- getLine
  putStrLn "Enter text to be encoded:"
  text <- getLine
  let encodedText = vigenere keyword $ map (toLower) text
  putStrLn encodedText
