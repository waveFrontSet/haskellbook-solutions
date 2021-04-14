module StopAndVowel where

-- 1.
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"


make3Tuples :: String -> String -> [(Char, Char, Char)]
make3Tuples xs ys = [ (x, y, z) | x <- xs, y <- ys, z <- xs ]

make3TuplesWithInitialP :: String -> String -> [(Char, Char, Char)]
make3TuplesWithInitialP xs = filter (\(x, _, _) -> x == 'p') . make3Tuples xs

nouns = ["Haskell", "Python", "Java", "C", "Ruby"]
verbs = ["rocks", "owns", "beats", "is better than"]

make3Tuples2 :: [a] -> [a] -> [(a, a, a)]
make3Tuples2 xs ys = [ (x, y, z) | x <- xs, y <- ys, z <- xs ]

-- 2. seekritFunc computes the average length of words in a string
seekritFunc :: String -> Double
seekritFunc x = sumOfLengthOfWords x / amountOfWords x
 where
  amountOfWords      = fromIntegral . length . words
  sumOfLengthOfWords = fromIntegral . sum . map length . words
