module Ch15MonoidSemigroup.SemigroupExercises where

import           Ch15MonoidSemigroup.QuickCheckHelpers
                                                ( semigroupAssoc )
import           Data.Monoid
import           Test.QuickCheck                ( Arbitrary
                                                , CoArbitrary
                                                , arbitrary
                                                , frequency
                                                , quickCheck
                                                )

-- 1.
data Trivial = Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  x <> _ = x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

type IdS = Identity String
type IdAssoc = IdS -> IdS -> IdS -> Bool

-- 3.
data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x y <> Two x' y' = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

type TwoSP = Two String (Product Integer)
type TwoAssoc = TwoSP -> TwoSP -> TwoSP -> Bool

-- 4.
data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three x y z <> Three x' y' z' = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

type ThreeSPS = Three String (Product Integer) (Sum Integer)
type ThreeAssoc = ThreeSPS -> ThreeSPS -> ThreeSPS -> Bool

-- 5.
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four x y z w <> Four x' y' z' w' =
    Four (x <> x') (y <> y') (z <> z') (w <> w')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return $ Four x y z w

type FourSPSL = Four String (Product Integer) (Sum Integer) [String]
type FourAssoc = FourSPSL -> FourSPSL -> FourSPSL -> Bool

-- 6.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj x <> BoolConj y = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return $ BoolConj x

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7.
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj x <> BoolDisj y = BoolDisj (x || y)

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    return $ BoolDisj x

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8.
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  _         <> s@(Snd _) = s
  s@(Snd _) <> _         = s
  _         <> fs        = fs

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ Fst x), (1, return $ Snd y)]

type OrSI = Or String Integer
type OrAssoc = OrSI -> OrSI -> OrSI -> Bool

-- 9.
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show = const "Combine"

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

type StoP = Combine String (Product Integer)
type CombineAssoc = StoP -> StoP -> StoP -> String -> Bool

combineAssoc :: CombineAssoc
combineAssoc x y z s =
  unCombine ((x <> y) <> z) s == unCombine (x <> (y <> z)) s

-- 10.
newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

instance Show (Comp a) where
  show = const "Comp"

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

type StoS = Comp String
type CompAssoc = StoS -> StoS -> StoS -> String -> Bool

compAssoc :: CompAssoc
compAssoc x y z s = unComp ((x <> y) <> z) s == unComp (x <> (y <> z)) s

-- 11.
data Validation a b = Failure a | Success b deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
  s@(Success _) <> _             = s
  _             <> s@(Success _) = s
  Failure x     <> Failure y     = Failure (x <> y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ Failure x), (1, return $ Success y)]

type ValSI = Validation String Int
type ValAssoc = ValSI -> ValSI -> ValSI -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck combineAssoc
  quickCheck compAssoc
  quickCheck (semigroupAssoc :: ValAssoc)
