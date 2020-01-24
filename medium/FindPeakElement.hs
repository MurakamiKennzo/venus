-- A peak element is an element that is greater than its neighbors.

-- Given an input array nums, where nums[i] ≠ nums[i+1], find a peak element and return its index.

-- The array may contain multiple peaks, in that case return the index to any one of the peaks is fine.

-- You may imagine that nums[-1] = nums[n] = -∞.

module FindPeakElement
  (
    findPeakElement
  ) where

findPeakElement :: (Ord a) => [a] -> Maybe Int
findPeakElement = findPeakElement' 0

findPeakElement' :: (Ord a) => Int -> [a] -> Maybe Int
findPeakElement' i a
  | i >= l = Nothing
  | i == l - 1 = if a !! (pred i) < a !! i then Just i else Nothing
  | i == 0 = if a !! i > a !! (succ i) then Just i else findPeakElement' (succ i) a
  | otherwise = if a !! i > a !! (succ i) && a !! (pred i) < a !! i then Just i else findPeakElement' (succ i) a
  where l = length a
