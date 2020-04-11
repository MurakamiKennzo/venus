-- Write a program to check whether a given number is an ugly number.

-- Ugly numbers are positive numbers whose prime factors only include 2, 3, 5.

module IsUgly
  (
    isUgly
  ) where

isUgly :: Int -> Bool
isUgly 1 = True
isUgly n
  | n `mod` 2 == 0 = isUgly $ n `div` 2
  | n `mod` 3 == 0 = isUgly $ n `div` 3
  | n `mod` 4 == 0 = isUgly $ n `div` 5
  | otherwise = False
