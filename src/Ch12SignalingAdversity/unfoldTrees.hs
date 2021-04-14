module UnfoldTrees where

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
  Nothing         -> Leaf
  Just (a, b, a') -> Node (unfold f a) b (unfold f a')

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
 where
  f x | x >= n    = Nothing
      | otherwise = Just (x + 1, x, x + 1)
