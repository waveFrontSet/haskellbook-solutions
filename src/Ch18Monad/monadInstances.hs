module MonadInstances where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- 1.
data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure = const NopeDotJpg
  (<*>) _ = const NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ = const NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return $ NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

-- 2.
data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight x) = PRight x
  fmap f (PLeft  x) = PLeft $ f x

instance Applicative (BahEither b) where
  pure = PLeft
  (<*>) (PRight x) _          = PRight x
  (<*>) _          (PRight x) = PRight x
  (<*>) (PLeft f)  (PLeft  x) = PLeft $ f x

instance Monad (BahEither b) where
  return = pure
  (>>=) (PRight x) _ = PRight x
  (>>=) (PLeft  x) f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [PLeft x, PRight y]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- 4.
data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

append :: List a -> List a -> List a
append Nil         xs = xs
append (Cons x xs) ys = Cons x $ append xs ys

instance Applicative List where
  pure = flip Cons Nil
  (<*>) Nil         _  = Nil
  (<*>) (Cons f fs) xs = append (f <$> xs) (fs <*> xs)

instance Monad List where
  return = pure
  (>>=) Nil         _ = Nil
  (>>=) (Cons x xs) f = append (f x) (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x   <- arbitrary
    lst <- arbitrary
    frequency [(1, return Nil), (2, return $ Cons x lst)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  let nope :: Nope (Int, String, Char)
      nope = undefined
      bahEither :: BahEither Int (Int, String, Char)
      bahEither = undefined
      identity :: Identity (Int, String, Char)
      identity = undefined
      list :: List (Int, String, Char)
      list = undefined
  quickBatch (applicative $ nope)
  quickBatch (monad $ nope)
  quickBatch (applicative $ bahEither)
  quickBatch (monad $ bahEither)
  quickBatch (applicative $ identity)
  quickBatch (monad $ identity)
  quickBatch (applicative $ list)
  quickBatch (monad $ list)
