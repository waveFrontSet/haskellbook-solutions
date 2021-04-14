module StandardFuncs where

myOr :: [Bool] -> Bool
myOr []          = False
myOr (True : _ ) = True
myOr (_    : xs) = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) | f x       = True
                 | otherwise = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y : ys) | x == y    = True
                  | otherwise = myElem x ys

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = any (x ==)

myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : ys) = (myReverse ys) ++ [x]

squish :: [[a]] -> [a]
squish []       = []
squish (x : ys) = x ++ squish ys

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []       = []
squishMap f (x : ys) = f x ++ squishMap f ys

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x : xs) | f x remainderMax == GT = x
                       | otherwise              = remainderMax
  where remainderMax = myMaximumBy f xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy (flip f)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
