-- Given an unsorted array nums, reorder it such that nums[0] < nums[1] > nums[2] < nums[3]....

module WiggleSort
  (
    wiggleSort
  ) where

import Data.List ( permutations
                 , find )
                 
wiggleSort :: (Ord a) => [a] -> Maybe [a]
wiggleSort = find wiggle . permutations
  where wiggle :: (Ord a) => [a] -> Bool
        wiggle [] = True
        wiggle [_] = True
        wiggle [a, b] = if a < b then True else False
        wiggle (a:b:c:xs) = if a < b && b > c then wiggle (c:xs) else False
