-- Given an unsorted array return whether an increasing subsequence of length 3 exists or not in the array.

-- Formally the function should:

-- Return true if there exists i, j, k 
-- such that arr[i] < arr[j] < arr[k] given 0 ≤ i < j < k ≤ n-1 else return false.

module IncreasingTriplet
  (
    increasingTriplet
  ) where

increasingTriplet :: (Ord a) => [a] -> Bool
increasingTriplet [] = False
increasingTriplet (x:xs) = (>= 2) . length . filter (> x) $ xs
