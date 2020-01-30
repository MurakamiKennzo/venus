-- Given an integer n, return the number of trailing zeroes in n!.

module TrailingZeroes
  (
    trailingZeroes
  ) where

trailingZeroes :: Int -> Int
trailingZeroes n
  | a == 0 = 0
  | otherwise = a + trailingZeroes a
  where a = n `div` 5
