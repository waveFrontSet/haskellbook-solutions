-- Exercises Chapter 6.5
module Exercises65 where

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  TisAn x == TisAn y = x == y

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  Two x y == Two x' y' = (x, y) == (x', y')

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  TisAnInt   x == TisAnInt   y = x == y
  TisAString x == TisAString y = x == y
  _            == _            = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  Pair x y == Pair x' y' = (x, y) == (x', y')

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple x y == Tuple x' y' = (x, y) == (x', y')

data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
  ThisOne x == ThisOne y = x == y
  ThatOne x == ThatOne y = x == y
  _         == _         = False

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello   x == Hello   y = x == y
  Goodbye x == Goodbye y = x == y
  _         == _         = False
