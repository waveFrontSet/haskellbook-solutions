module Validation where

import           Test.QuickCheck                ( Arbitrary
                                                , arbitrary
                                                )
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Gen            ( oneof )

data Validation e a = Failure e | Success a deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success x) = Success $ f x

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Success f) (Success x ) = Success $ f x
  (<*>) (Success _) (Failure e ) = Failure e
  (<*>) (Failure e) (Success _ ) = Failure e
  (<*>) (Failure e) (Failure e') = Failure $ e <> e'

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    oneof [return $ Failure e, return $ Success a]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
  let x :: Validation [Int] (String, Char, Int)
      x = undefined
  quickBatch (applicative $ x)
