module IterateAndUnfold where

-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f start = start : (go start) where go x = (f x) : (go (f x))

-- 2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Just (y, x') -> y : (myUnfoldr f x')
  Nothing      -> []

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, f a)) x
