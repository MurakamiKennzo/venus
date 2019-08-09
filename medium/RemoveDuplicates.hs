-- Given a sorted array nums, remove the duplicates in-place such that duplicates appeared at most twice and return the new length.

module RemoveDuplicates
  (
    removeDuplicates
  ) where

removeDuplicates :: [Int] -> Int
removeDuplicates [] = 0
removeDuplicates [x] = 1
removeDuplicates [x, y] = 2
removeDuplicates (x:y:z:xs) = (if x == y && y == z then 0 else 1) + removeDuplicates (y:z:xs) 
