module Ch08Recursion.ChapterExercises where

import           Data.List                      ( intersperse )

recurseSum :: (Eq a, Num a) => a -> a
recurseSum 0 = 0
recurseSum n = n + recurseSum (n - 1)

recurseMult :: (Integral a) => a -> a -> a
recurseMult 0 _ = 0
recurseMult n m = m + recurseMult (n - 1) m

data DividedResult = Result Integer | DividedByZero deriving (Show)

dividedBy :: Integer -> Integer -> DividedResult
dividedBy _   0     = DividedByZero
dividedBy num denom = Result $ go (abs num) (abs denom) 0
 where
  go n d count | signum num == signum denom && n < d = count
               | signum num /= signum denom && n < d = negate count
               | otherwise                           = go (n - d) d (count + 1)

mc91 :: Integer -> Integer
mc91 n | n > 100   = n - 10
       | otherwise = mc91 $ mc91 $ n + 11


digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits num = go num [] where
  go n xs
    | n < 10
    = n : xs
    | otherwise
    = let ones      = n `mod` 10
          remainder = n `div` 10
      in  go remainder (ones : xs)

wordNumber :: Int -> String
wordNumber = join . digitList
 where
  join      = concat . intersperse "-"
  digitList = map digitToWord . digits
