module QuickCheckTests where

import           Data.List                      ( sort )
import           Test.QuickCheck

-- 1.
half :: (Fractional a) => a -> a
half x = x / 2

doubleGen :: Gen Double
doubleGen = arbitrary

halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half

prop_halfIdentity :: Property
prop_halfIdentity = forAll doubleGen (\x -> x == halfIdentity x)

-- 2.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
 where
  go _ status@(_      , False) = status
  go y (       Nothing, t    ) = (Just y, t)
  go y (       Just x , _    ) = (Just y, x >= y)

intListGen :: Gen [Int]
intListGen = arbitrary

prop_listOrdered :: Property
prop_listOrdered = forAll intListGen (\xs -> listOrdered $ sort xs)

-- 3.
plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

intTupleGen :: Gen (Int, Int)
intTupleGen = arbitrary

intTripleGen :: Gen (Int, Int, Int)
intTripleGen = arbitrary

prop_plusAssociative :: Property
prop_plusAssociative =
  forAll intTripleGen (\(x, y, z) -> plusAssociative x y z)

prop_plusCommutative :: Property
prop_plusCommutative = forAll intTupleGen (\(x, y) -> plusCommutative x y)

-- 4.
multAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Num a, Eq a) => a -> a -> Bool
multCommutative x y = x * y == y * x

prop_multAssociative :: Property
prop_multAssociative =
  forAll intTripleGen (\(x, y, z) -> multAssociative x y z)

prop_multCommutative :: Property
prop_multCommutative = forAll intTupleGen (\(x, y) -> multCommutative x y)


main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multAssociative
  quickCheck prop_multCommutative
