module LibForEither where

-- 1.
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
 where
  f (Left  x) acc = x : acc
  f (Right _) acc = acc

-- 2.
rights' :: [Either a b] -> [b]
rights' = foldr f []
 where
  f (Left  _) acc = acc
  f (Right x) acc = x : acc

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left  _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left  x) = f x
either' _ g (Right y) = g y

-- 6.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (Just . f)
