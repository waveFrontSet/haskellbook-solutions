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
