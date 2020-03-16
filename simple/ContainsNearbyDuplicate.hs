-- Given an array of integers and an integer k, find out whether there are two distinct indices i and j in the array such that nums[i] = nums[j] and the absolute difference between i and j is at most k.

module ContainsNearbyDuplicate
  (
    containsNearbyDuplicate
  ) where

containsNearbyDuplicate :: (Ord a) => [a] -> Int -> Bool
containsNearbyDuplicate [] _ = False
containsNearbyDuplicate (x:xs) n
  | n < 0 = False
  | n == 0 = True
  | n > 0 = ((x `elem`) . take n $ xs) || containsNearbyDuplicate xs n
