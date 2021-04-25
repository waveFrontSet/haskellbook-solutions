module EitherMonad where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First  x) = First x
  fmap f (Second x) = Second $ f x

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First f)  _          = First f
  (<*>) _          (First  x) = First x
  (<*>) (Second f) (Second x) = Second $ f x

instance Monad (Sum a) where
  return = pure
  (>>=) (First  x) _ = First x
  (>>=) (Second x) f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ First x), (2, return $ Second y)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

main :: IO ()
main = do
  let sumMonad :: Sum Int (Integer, String, Char)
      sumMonad = undefined
  quickBatch (functor $ sumMonad)
  quickBatch (applicative $ sumMonad)
  quickBatch (monad $ sumMonad)
