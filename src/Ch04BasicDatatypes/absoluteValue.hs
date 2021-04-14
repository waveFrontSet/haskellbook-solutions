module AbsoluteValue where

-- Chapter exercise 9
myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else negate x

-- Chapter exercise 10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f v w = ((snd v, snd w), (fst v, fst w))
