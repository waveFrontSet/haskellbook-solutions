module Folds where

-- 1.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (== x)) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = myAny (== x)

-- 4.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

-- 7.
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (a : as) =
  foldr (\x acc -> if f x acc == GT then x else acc) a as

-- 11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy (flip f)
