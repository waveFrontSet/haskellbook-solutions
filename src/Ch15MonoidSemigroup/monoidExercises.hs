module Ch15MonoidSemigroup.MonoidExercises where

import           Ch15MonoidSemigroup.QuickCheckHelpers
import           Data.Monoid
import           Test.QuickCheck

-- 1.
data Trivial = Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

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

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

type TwoSP = Two String (Product Integer)
type TwoAssoc = TwoSP -> TwoSP -> TwoSP -> Bool

-- 4.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj x <> BoolConj y = BoolConj (x && y)

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return $ BoolConj x

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 5.
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj x <> BoolDisj y = BoolDisj (x || y)

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    return $ BoolDisj x

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 6.
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show = const "Combine"

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ const mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

type StoP = Combine String (Product Integer)
type CombineAssoc = StoP -> StoP -> StoP -> String -> Bool

combineAssoc :: CombineAssoc
combineAssoc x y z s =
  unCombine ((x <> y) <> z) s == unCombine (x <> (y <> z)) s

combineLeftId :: StoP -> String -> Bool
combineLeftId x s = unCombine (x <> mempty) s == unCombine x s

combineRightId :: StoP -> String -> Bool
combineRightId x s = unCombine (mempty <> x) s == unCombine x s

-- 7.
newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id

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

compLeftId :: StoS -> String -> Bool
compLeftId x s = unComp (x <> mempty) s == unComp x s

compRightId :: StoS -> String -> Bool
compRightId x s = unComp (mempty <> x) s == unComp x s

-- 8.
newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Show (Mem s a) where
  show = const "Mem"

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem compFG   where
    compFG s =
      let (x, s' ) = g s
          (y, s'') = f s'
      in  (x <> y, s'')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Mem s a) where
  arbitrary = do
    f <- arbitrary
    return $ Mem f

type MemSS = Mem String String
memAssoc :: MemSS -> MemSS -> MemSS -> String -> Bool
memAssoc x y z s = runMem ((x <> y) <> z) s == runMem (x <> (y <> z)) s

memLeftId :: MemSS -> String -> Bool
memLeftId x s = runMem (mempty <> x) s == runMem x s

memRightId :: MemSS -> String -> Bool
memRightId x s = runMem (x <> mempty) s == runMem x s

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (monoidLeftIdentity :: IdS -> Bool)
  quickCheck (monoidRightIdentity :: IdS -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: TwoSP -> Bool)
  quickCheck (monoidRightIdentity :: TwoSP -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck combineAssoc
  quickCheck combineLeftId
  quickCheck combineRightId
  quickCheck compAssoc
  quickCheck compLeftId
  quickCheck compRightId
  quickCheck memAssoc
  quickCheck memLeftId
  quickCheck memRightId
