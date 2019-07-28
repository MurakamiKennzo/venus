-- Implement int sqrt(int x).

-- Compute and return the square root of x, where x is guaranteed to be a non-negative integer.

-- Since the return type is an integer, the decimal digits are truncated and only the integer part of the result is returned.

module MySqrt
  (
    mySqrt
  ) where

mySqrt :: Int -> Int
mySqrt = mySqrt' 1

mySqrt' :: Int -> Int -> Int
mySqrt' x n
  | sqx > n = x - 1
  | sqx == n = x
  | otherwise = mySqrt' (x + 1) n
  where sqx = x ^ 2
