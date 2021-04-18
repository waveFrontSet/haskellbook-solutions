module Ch16Functor.FunctorInstances where

import           Ch16Functor.QuickCheckHelpers
import           Test.QuickCheck

-- 1.
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

idCompose :: Identity Int -> Bool
idCompose = functorCompose (+ 1) (* 3)


main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity String -> Bool)
  quickCheck (idCompose)
