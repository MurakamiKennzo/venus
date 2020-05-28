-- Given an integer array nums, find the sum of the elements between indices i and j (i ≤ j), inclusive.

module SumRange
  (
    sumRange
  ) where

sumRange :: (Num a) => (Int, Int) -> [a] -> a
sumRange (a, b) = sum . take (b - (max 0 a) + 1) . drop a
