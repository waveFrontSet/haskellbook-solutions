module Natural where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat n | n < 0     = Nothing
               | otherwise = Just (go n)
 where
  go 0 = Zero
  go x = Succ (go (x - 1))
