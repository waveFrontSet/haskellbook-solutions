module Instances where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- 1.
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f g) (Pair x y) = Pair (f x) $ g y

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

-- 2.
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x $ f y

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two m f) (Two m' x) = Two (m <> m') $ f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- 3.
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y $ f z

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three m n f) (Three m' n' x) = Three (m <> m') (n <> n') $ f x

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c ) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 4.
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' m f g) (Three' m' x y) = Three' (m <> m') (f x) $ g y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 5.
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c ) where
  fmap f (Four x y z w) = Four x y z $ f w

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four m n p f) (Four m' n' p' x) =
    Four (m <> m') (n <> n') (p <> p') $ f x

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return $ Four x y z w

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- 6.
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x x' x'' y) = Four' x x' x'' $ f y

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' m n p f) (Four' m' n' p' x) =
    Four' (m <> m') (n <> n') (p <> p') $ f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return $ Four' x y z w

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

main :: IO ()
main = do
  let pair :: Pair (Int, String, Char)
      pair = undefined
      two :: Two [Int] (Int, String, Char)
      two = undefined
      three :: Three [Int] String (Int, String, Char)
      three = undefined
      three' :: Three' [Int] (Int, String, Char)
      three' = undefined
      four :: Four [Int] String [String] (Int, String, Char)
      four = undefined
      four' :: Four' [Int] (Int, String, Char)
      four' = undefined
  quickBatch (applicative $ pair)
  quickBatch (applicative $ two)
  quickBatch (applicative $ three)
  quickBatch (applicative $ three')
  quickBatch (applicative $ four)
  quickBatch (applicative $ four')
