-- Given a set of distinct integers, nums, return all possible subsets (the power set).

-- Note: The solution set must not contain duplicate subsets.

module Subsets 
  (
    subsets
  ) where

subsets :: (Num a) => [a] -> [[a]]
subsets [] = return []
subsets (x:xs) = let y = subsets xs in y ++ map (x:) y
