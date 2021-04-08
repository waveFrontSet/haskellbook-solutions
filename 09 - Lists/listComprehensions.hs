module ListComprehensions where

mySqr = [ x ^ 2 | x <- [1 .. 5] ]
myCube = [ y ^ 3 | y <- [1 .. 5] ]

-- 1.
myTuples = [ (x, y) | x <- mySqr, y <- myCube ]

-- 2.
myTuples2 = [ (x, y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]

-- 3.
result = length myTuples2
