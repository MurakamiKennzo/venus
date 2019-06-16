-- Given an array of integers nums sorted in ascending order, find the starting and ending position of a given target value.

-- If the target is not found in the array, return [-1, -1].

module SearchRange 
  (
    searchRange
  ) where

import Data.List (elemIndices)

searchRange :: [Int] -> Int -> [Int]
searchRange a b = fomrmat $ b `elemIndices` a
  where fomrmat :: [Int] -> [Int]
        fomrmat [] = [-1, -1]
        fomrmat [a] = [a, -1]
        fomrmat xs = [head xs, last xs]
