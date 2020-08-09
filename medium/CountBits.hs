-- Given a non negative integer number num. For every numbers i in the range 0 ≤ i ≤ num calculate the number of 1's in their binary representation and return them as an array.

module CountBits
  (
    countBits
  ) where

import Data.Bits ( popCount )

countBits :: Int -> [Int]
countBits n = [ popCount x | x <- [0 .. n] ]
