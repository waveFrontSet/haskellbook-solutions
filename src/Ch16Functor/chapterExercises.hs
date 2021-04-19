{-# LANGUAGE FlexibleInstances #-}
module ChapterExercises where

import           Ch16Functor.QuickCheckHelpers
import           Test.QuickCheck

funcCompose :: (Num a, Eq (f a), Functor f) => f a -> Bool
funcCompose = functorCompose (+ 1) (* 3)

-- 1.
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk  x) = Desk x
  fmap f (Bloor x) = Bloor $ f x

-- 2.
data K a b = K a

instance Functor (K a) where
  fmap _ (K x) = K x

-- 3.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

-- 4.
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst $ f x

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
