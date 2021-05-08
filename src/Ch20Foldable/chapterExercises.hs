module ChapterExercises where

-- 1.
data Constant a b = Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant x) = f x

-- 2.
data Two a b = Two a b
  deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two _ y) = f y

-- 3.
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

-- 4.
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ y y') = f y <> f y'

-- 5.
data Four' a b = Four' a b b b
  deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ x y z) = f x <> f y <> f z

filterF
  :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
