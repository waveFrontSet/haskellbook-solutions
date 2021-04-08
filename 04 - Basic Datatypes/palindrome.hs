-- Chapter exercise 8
module Palindrome where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x
