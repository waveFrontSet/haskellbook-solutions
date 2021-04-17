module QuickCheckTests where

import           Ch11AlgebraicDatatypes.Language
                                                ( capitalizeWord )
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
binaryOpAssociative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
binaryOpAssociative binOp x y z =
  x `binOp` (y `binOp` z) == (x `binOp` y) `binOp` z

binaryOpCommutative :: Eq a => (a -> a -> a) -> a -> a -> Bool
binaryOpCommutative binOp x y = binOp x y == binOp y x

multAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
multAssociative = binaryOpAssociative (*)

multCommutative :: (Num a, Eq a) => a -> a -> Bool
multCommutative = binaryOpCommutative (*)

prop_multAssociative :: Property
prop_multAssociative =
  forAll intTripleGen (\(x, y, z) -> multAssociative x y z)

prop_multCommutative :: Property
prop_multCommutative = forAll intTupleGen (\(x, y) -> multCommutative x y)

-- 5.
divisibleTuple :: Gen (Int, Int)
divisibleTuple = do
  x <- arbitrary
  y <- arbitrary `suchThat` (/= 0)
  return (x, y)

quotRemRelation :: Integral a => a -> a -> Bool
quotRemRelation x y = (quot x y) * y + (rem x y) == x

divModRelation :: Integral a => a -> a -> Bool
divModRelation x y = (div x y) * y + (mod x y) == x

prop_quotRemRelation :: Property
prop_quotRemRelation = forAll divisibleTuple (\(x, y) -> quotRemRelation x y)

prop_divModRelation :: Property
prop_divModRelation = forAll divisibleTuple (\(x, y) -> divModRelation x y)

-- 6.
positiveIntTriple :: Gen (Int, Int, Int)
positiveIntTriple = do
  x <- arbitrary `suchThat` (> 0)
  y <- arbitrary `suchThat` (> 0)
  z <- arbitrary `suchThat` (> 0)
  return (x, y, z)

positiveIntTuple :: Gen (Int, Int)
positiveIntTuple = do
  x <- arbitrary `suchThat` (> 0)
  y <- arbitrary `suchThat` (> 0)
  return (x, y)

prop_expAssociative :: Property
prop_expAssociative =
  forAll positiveIntTriple (\(x, y, z) -> binaryOpAssociative (^) x y z)

prop_expCommutative :: Property
prop_expCommutative =
  forAll positiveIntTuple (\(x, y) -> binaryOpCommutative (^) x y)

-- 7.
prop_reverseIsInvolution :: Property
prop_reverseIsInvolution =
  forAll intListGen (\xs -> xs == reverse (reverse xs))

-- 8.
prop_apply :: Property
prop_apply = forAll (arbitrary :: (Gen Int)) (\x -> (id $ x) == id x)

prop_composition :: Property
prop_composition = forAll (arbitrary :: (Gen Int)) (\x -> x == (id . id) x)

-- 9.
prop_foldAndConcat :: Property
prop_foldAndConcat = forAll (arbitrary :: (Gen ([Int], [Int])))
                            (\(xs, ys) -> foldr (:) xs ys == (++) xs ys)
prop_foldAndConcat2 :: Property
prop_foldAndConcat2 =
  forAll (arbitrary :: Gen [[Int]]) (\xs -> foldr (++) [] xs == concat xs)

-- 10.
intListTuple :: Gen (Int, [Int])
intListTuple = arbitrary

f1 :: Int -> [a] -> Bool
f1 n xs = length (take n xs) == n

prop_lengthEqualsTakenElements :: Property
prop_lengthEqualsTakenElements = forAll (intListTuple) (\(n, xs) -> f1 n xs)

-- 11.
f2 :: (Read a, Show a) => a -> a
f2 = read . show

prop_roundtripIsIdentity :: Property
prop_roundtripIsIdentity = forAll doubleGen (\x -> f2 x == x)

-- Failure
square :: Num a => a -> a
square x = x * x

squareIdentity :: Double -> Double
squareIdentity = square . sqrt

prop_squareAndSqrtAreInverses :: Property
prop_squareAndSqrtAreInverses = forAll doubleGen (\x -> squareIdentity x == x)
-- Fails because of floating point precision

-- Idempotence
twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

capWordIsIdempotent :: String -> Bool
capWordIsIdempotent x =
  (capitalizeWord x == twice capitalizeWord x)
    && (capitalizeWord x == fourTimes capitalizeWord x)

sortIsIdempotent :: String -> Bool
sortIsIdempotent x = (sort x == twice sort x) && (sort x == fourTimes sort x)

prop_capWordIsIdempotent :: Property
prop_capWordIsIdempotent =
  forAll (arbitrary :: Gen String) (\x -> capWordIsIdempotent x)

prop_sortIsIdempotent :: Property
prop_sortIsIdempotent =
  forAll (arbitrary :: Gen String) (\x -> sortIsIdempotent x)

main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multAssociative
  quickCheck prop_multCommutative
  quickCheck prop_quotRemRelation
  quickCheck prop_divModRelation
  quickCheck prop_expAssociative
  quickCheck prop_expCommutative
  quickCheck prop_reverseIsInvolution
  quickCheck prop_foldAndConcat
  quickCheck prop_foldAndConcat2
  quickCheck prop_lengthEqualsTakenElements
  quickCheck prop_roundtripIsIdentity
  quickCheck prop_squareAndSqrtAreInverses
  quickCheck prop_capWordIsIdempotent
  quickCheck prop_sortIsIdempotent
