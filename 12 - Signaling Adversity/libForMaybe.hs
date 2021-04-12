module LibForMaybe where

-- 1.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2.
mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe y _ Nothing  = y
mayybe _ f (Just x) = f x

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybe x id

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = mayybe [] (: [])

-- 5.
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f []
 where
  f Nothing  xs = xs
  f (Just x) xs = x : xs

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs | any isNothing xs = Nothing
             | otherwise        = Just (foldr f [] xs)
 where
  f (Just x) acc = x : acc
  f Nothing  acc = acc
