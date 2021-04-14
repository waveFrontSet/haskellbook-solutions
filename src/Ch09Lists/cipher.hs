module Cipher where
import           Data.Char
import           Data.List                      ( intercalate )

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

-- Chapter 13
main :: IO ()
main = do
  putStrLn "Enter offset:"
  offsetStr <- getLine
  let offsetInt :: Int
      offsetInt = read offsetStr
  putStrLn "Enter text to be encoded:"
  text <- getLine
  let splitIntoWords = words . map (toLower)
      encodedText =
        intercalate " " $ map (caesar offsetInt) $ splitIntoWords text
  putStrLn encodedText
