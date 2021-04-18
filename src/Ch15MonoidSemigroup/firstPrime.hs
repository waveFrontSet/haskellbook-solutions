module Ch15MonoidSemigroup.FirstPrime where

import           Ch15MonoidSemigroup.Optional   ( Optional(..) )
import           Ch15MonoidSemigroup.QuickCheckHelpers
import           Test.QuickCheck

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (  First' Nada    ) y = y
  (<>) x@(First' (Only _)) _ = x

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return $ First' Nada), (2, return $ First' (Only x))]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend
type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
