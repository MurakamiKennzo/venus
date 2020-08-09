-- Given an integer (signed 32 bits), write a function to check whether it is a power of 4.

module IsPowerOfFour
  (
    isPowerOfFour
  ) where

isPowerOfFour :: Int -> Bool
isPowerOfFour 0 = False
isPowerOfFour 1 = True
isPowerOfFour n
  | b == 0 = isPowerOfFour a
  | otherwise = False
  where (a, b) = n `divMod` 4
