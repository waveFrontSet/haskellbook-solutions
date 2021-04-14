-- p. 335
module FilterLists where

-- 1.
isMultipleOf3 :: (Integral a) => a -> Bool
isMultipleOf3 x = x `mod` 3 == 0
-- filter isMultipleOf3 [1..30]

-- 2.
-- length $ filter isMultipleOf3 [1..30]

-- 3.
myFilter :: String -> [String]
myFilter = filter (not . isArticle) . words
 where
  articles = ["a", "an", "the"]
  isArticle s = s `elem` articles
