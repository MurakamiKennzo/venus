-- Given a range [m, n] where 0 <= m <= n <= 2147483647, return the bitwise AND of all numbers in this range, inclusive.

module RangeBitwiseAnd
  (
    rangeBitwiseAnd
  ) where

import Data.Bits ( shiftL
                 , shiftR )

rangeBitwiseAnd :: Int -> Int -> Int
rangeBitwiseAnd m n
  | m == n = m
  | otherwise = (rangeBitwiseAnd (m `shiftR` 1) (n `shiftR` 1)) `shiftL` 1
