-- Given an integer, write a function to determine if it is a power of two.

module IsPowerOfTwo
  (
    isPowerOfTwo
  ) where

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n
  | n <= 0 = False
  | n == 1 = True
  | odd n = False
  | otherwise = isPowerOfTwo $ n `div` 2
