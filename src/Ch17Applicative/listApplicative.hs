module ListApplicative where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

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

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x   <- arbitrary
    lst <- arbitrary
    frequency [(1, return Nil), (2, return $ Cons x lst)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'   where
    xs' = let (ZipList' l) = xs in take 3000 l
    ys' = let (ZipList' l) = ys in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

appendToZip :: a -> ZipList' a -> ZipList' a
appendToZip x (ZipList' xs) = ZipList' $ x : xs

instance Applicative ZipList' where
  pure = ZipList' . repeat
  (<*>) (ZipList' []) _             = ZipList' []
  (<*>) _             (ZipList' []) = ZipList' []
  (<*>) (ZipList' (f : fs)) (ZipList' (x : xs)) =
    appendToZip (f x) $ ZipList' fs <*> ZipList' xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    xs <- arbitrary
    return $ ZipList' xs

main :: IO ()
main = do
  let x :: List (Int, String, Char)
      x = undefined
      y :: ZipList' (Int, String, Char)
      y = undefined
  quickBatch (applicative $ x)
  quickBatch (applicative $ y)
