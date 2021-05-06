module LibFuncs where

import           Data.Monoid                    ( Product(..)
                                                , Sum(..)
                                                )

-- 1.
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- 3.
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = foldr ((||) . (== a)) False

-- 4.
fld :: Ord a => (a -> a -> a) -> a -> Maybe a -> Maybe a
fld _    x Nothing = Just x
fld comp x y       = comp <$> Just x <*> y

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr (fld min) Nothing

-- 5.
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr (fld max) Nothing

-- 6.
null :: (Foldable t) => t a -> Bool
null = foldr (const . const False) True

-- 7.
length :: (Foldable t) => t a -> Int
length = foldr ((+) . const 1) 0

-- 8.
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- 9.
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- 10.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty
