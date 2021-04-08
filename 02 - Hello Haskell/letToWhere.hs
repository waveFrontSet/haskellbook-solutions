-- 2.10 Exercises
module Exercise where

sol1 = x * 3 + y
 where
  x = 3
  y = 1000

sol2 = x * 5
 where
  x = 10 * 5 + y
  y = 10

sol3 = z / x + y
 where
  x = 7
  y = negate x
  z = y * 10
