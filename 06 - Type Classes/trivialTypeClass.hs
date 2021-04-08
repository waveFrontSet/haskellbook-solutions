-- Trivial example for Type Classes
module TrivialTypeClass where

data Trivial = Trivial'
instance Eq Trivial where
  Trivial' == Trivial' = True
