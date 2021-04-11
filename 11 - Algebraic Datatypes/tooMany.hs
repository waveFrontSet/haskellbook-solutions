{-# LANGUAGE FlexibleInstances #-}
module TooManyFuncs where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (x, s) = length s > 42 || tooMany x

instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany (x + y)

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)
