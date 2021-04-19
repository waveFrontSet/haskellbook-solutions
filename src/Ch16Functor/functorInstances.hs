module Ch16Functor.FunctorInstances where

import           Ch16Functor.QuickCheckHelpers
import           Test.QuickCheck

funcCompose :: (Num a, Eq (f a), Functor f) => f a -> Bool
funcCompose = functorCompose (+ 1) (* 3)

-- 1.
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

-- 2.
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

-- 3.
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x $ f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

-- 4.
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y $ f z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c ) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

-- 5.
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

-- 6.
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c ) where
  fmap f (Four x y z w) = Four x y z $ f w

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return $ Four x y z w

-- 7.
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x x' x'' y) = Four' x x' x'' $ f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return $ Four' x y z w

-- 8.
-- data Trivial = Trivial can't be a functor because it has the wrong kind:
-- a Functor needs to be of kind * -> * while Trivial has kind *.

-- p. 654
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers x) = Yeppers $ f x

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return LolNope), (1, return $ Yeppers x)]

main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity String -> Bool)
  quickCheck (funcCompose :: Identity Int -> Bool)
  quickCheck (functorIdentity :: Pair String -> Bool)
  quickCheck (funcCompose :: Pair Int -> Bool)
  quickCheck (functorIdentity :: Two Int String -> Bool)
  quickCheck (funcCompose :: Two String Int -> Bool)
  quickCheck (functorIdentity :: Three Int String Int -> Bool)
  quickCheck (funcCompose :: Three Int String Int -> Bool)
  quickCheck (functorIdentity :: Three' String Int -> Bool)
  quickCheck (funcCompose :: Three' String Int -> Bool)
  quickCheck (functorIdentity :: Four Int String Int Int -> Bool)
  quickCheck (funcCompose :: Four Int String Int Int -> Bool)
  quickCheck (functorIdentity :: Four' String Int -> Bool)
  quickCheck (funcCompose :: Four' String Int -> Bool)
  quickCheck (functorIdentity :: Possibly String -> Bool)
  quickCheck (funcCompose :: Possibly Int -> Bool)
