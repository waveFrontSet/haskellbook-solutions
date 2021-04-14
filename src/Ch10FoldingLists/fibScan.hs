module FibScan where

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN n = fibs !! n


-- 1.
fibs2 = take 20 fibs

-- 2.
fibs3 = takeWhile (< 100) fibs

-- 3.
factorials :: [Integer]
factorials = scanl (*) 1 [1 ..]

factorial :: Int -> Integer
factorial n = factorials !! n
