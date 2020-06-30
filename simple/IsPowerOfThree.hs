-- Given an integer, write a function to determine if it is a power of three.

module IsPowerOfThree
  (
    isPowerOfThree
  ) where

isPowerOfThree :: Int -> Bool
isPowerOfThree n
  | n <= 0 = False
  | n == 1 = True
  | b /= 0 = False
  | otherwise = isPowerOfThree a
  where (a, b) = n `divMod` 3
