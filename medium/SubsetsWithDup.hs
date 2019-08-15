-- Given a collection of integers that might contain duplicates, nums, return all possible subsets (the power set).

-- Note: The solution set must not contain duplicate subsets.

module SubsetsWithDup
  (
    subsetsWithDup
  ) where

import Data.List (nub)

subsetsWithDup :: [Int] -> [[Int]]
subsetsWithDup = nub . subsets

subsets :: [Int] -> [[Int]]
subsets [] = return []
subsets (x:xs) = let y = subsets xs
                 in  y <> map (x:) y
