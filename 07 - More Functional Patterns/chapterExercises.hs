module ChapterExercises where

-- 1.
tensDigit :: Integral a => a -> a
tensDigit x = d
 where
  (xLast, _) = x `divMod` 10
  d          = xLast `mod` 10

hunsDigit :: Integral a => a -> a
hunsDigit = (`mod` 10) . fst . (`divMod` 100)

-- 2.
foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
  True  -> y
  False -> x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b | b         = y
                | otherwise = x

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 5.
roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

-- 6.
roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 = read . show
-- print ((roundTrip2 4) :: Integer)
