-- Given a non-empty array nums containing only positive integers, find if the array can be partitioned into two subsets such that the sum of elements in both subsets is equal.

module CanPartition
  (
    canPartition
  ) where

canPartition :: [Int] -> Bool
canPartition xs
  | length xs < 2 = False
  | b /= 0 = False 
  | otherwise = canPartition' (length xs - 1) a xs
  where (a, b) = sum xs `divMod` 2

canPartition' :: Int -> Int -> [Int] -> Bool
canPartition' i c xs
  | c < 0 = False
  | c == 0 = True
  | i == 0 = xs !! 0 == c
  | otherwise = canPartition' (i - 1) c xs || canPartition' (i - 1) (c - xs !! i) xs
