{-# LANGUAGE FlexibleInstances #-}
module ChapterExercises where

import           Ch16Functor.QuickCheckHelpers
import           Test.QuickCheck

funcCompose :: (Num a, Eq (f a), Functor f) => f a -> Bool
funcCompose = functorCompose (+ 1) (* 3)

-- 1.
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk  x) = Desk x
  fmap f (Bloor x) = Bloor $ f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return Finance), (1, return $ Desk x), (1, return $ Bloor y)]

-- 2.
data K a b = K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

instance Arbitrary a => Arbitrary (K a b) where
  arbitrary = do
    x <- arbitrary
    return $ K x

-- 3.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K (f b)

instance Arbitrary (f b a) => Arbitrary (Flip f a b) where
  arbitrary = do
    x <- arbitrary
    return $ Flip x


-- 4.
data EvilGoateeConst a b = GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    x <- arbitrary
    return $ GoatyConst x

-- 5.
data LiftItOut f a = LiftItOut (f a)

instance Functor g => Functor (LiftItOut g) where
  fmap f (LiftItOut x) = LiftItOut $ fmap f x

-- 6.
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f' (DaWrappa x y) = DaWrappa (fmap f' x) (fmap f' y)

-- 7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f' (IgnoringSomething x y) = IgnoringSomething x $ fmap f' y

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y $ fmap f z

-- 9.
data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil          = Nil
  fmap f (Cons a lst) = Cons (f a) $ fmap f lst

-- 10.
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat x      ) = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11.
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s x) = Print s $ f x
  fmap f (Read g   ) = Read $ f . g

instance Eq a => Eq (TalkToMe a) where
  Halt        == Halt          = True
  (Print s x) == (Print s' x') = s == s' && x == x'
  -- Just checking equality on some values for good measure...
  (Read f) == (Read g) = f "" == g "" && f "a" == g "a" && f "z" == g "z"
  _           == _             = False

instance Show a => Show (TalkToMe a) where
  show Halt        = "Halt"
  show (Print s x) = "Print " ++ show s ++ show x
  -- Well, that's better than nothing.
  show (Read f   ) = "Read f where f('') = " ++ show (f "")

instance Arbitrary a => Arbitrary (TalkToMe a) where
  arbitrary = do
    s <- arbitrary
    x <- arbitrary
    f <- arbitrary
    frequency [(1, return Halt), (1, return $ Print s x), (1, return $ Read f)]


main :: IO ()
main = do
  quickCheck (functorIdentity :: TalkToMe String -> Bool)
  quickCheck (funcCompose :: TalkToMe Int -> Bool)
  quickCheck (functorIdentity :: Quant Int String -> Bool)
  quickCheck (funcCompose :: Quant Int Int -> Bool)
  quickCheck (functorIdentity :: K Int String -> Bool)
  quickCheck (funcCompose :: K Int Int -> Bool)
  quickCheck (functorIdentity :: Flip K String Int -> Bool)
  quickCheck (funcCompose :: Flip K String Int -> Bool)
  quickCheck (functorIdentity :: EvilGoateeConst Int Int -> Bool)
  quickCheck (funcCompose :: EvilGoateeConst Int Int -> Bool)
