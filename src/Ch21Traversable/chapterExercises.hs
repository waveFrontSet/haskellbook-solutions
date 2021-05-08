{-# LANGUAGE FlexibleContexts #-}
module ChapterExercises where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Traversable Identity where
  traverse f (Identity x) = fmap Identity $ f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

data S n a = S (n a) a
  deriving (Eq, Show)

instance ( Functor n , Arbitrary (n a) , Arbitrary a ) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n , Testable (n Property) , Eq a , Eq (n a) , EqProp a) => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S fx x) = S (fmap f fx) $ f x

instance Foldable n => Foldable (S n) where
  foldMap f (S fx x) = foldMap f fx <> f x

instance Traversable n => Traversable (S n) where
  traverse f (S fx x) = S <$> traverse f fx <*> f x

data Tree a = Empty
        | Leaf a
        | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty         = Empty
  fmap f (Leaf x     ) = Leaf $ f x
  fmap f (Node t x t') = Node (fmap f t) (f x) $ fmap f t'

instance Foldable Tree where
  foldMap _ Empty         = mempty
  foldMap f (Leaf x     ) = f x
  foldMap f (Node t x t') = (foldMap f t) <> f x <> (foldMap f t')

instance Traversable Tree where
  traverse _ Empty         = pure Empty
  traverse f (Leaf x     ) = Leaf <$> f x
  traverse f (Node t x t') = Node <$> traverse f t <*> f x <*> traverse f t'

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    x  <- arbitrary
    t  <- arbitrary
    t' <- arbitrary
    frequency
      [(1, return Empty), (1, return $ Leaf x), (1, return $ Node t x t')]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main = do
  let identity :: Identity (Int, String, String)
      identity = undefined
      s :: S [] (Int, String, String)
      s = undefined
      tree :: Tree (Int, String, String)
      tree = undefined
  quickBatch (traversable $ identity)
  quickBatch (traversable $ s)
  quickBatch (traversable $ tree)
