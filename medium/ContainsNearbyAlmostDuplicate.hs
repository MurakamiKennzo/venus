-- Given an array of integers, find out whether there are two distinct indices i and j in the array such that the absolute difference between nums[i] and nums[j] is at most t and the absolute difference between i and j is at most k.

module ContainsNearbyAlmostDuplicate
  (
    containsNearbyAlmostDuplicate
  ) where

containsNearbyAlmostDuplicate :: (Num a, Ord a) => [a] -> Int -> a -> Bool
containsNearbyAlmostDuplicate [] _ _ = False
containsNearbyAlmostDuplicate (x:xs) n t
  | n < 0 = False
  | n == 0 = if t /= 0 then False else True
  | otherwise = let ys = take n xs
                in  any ((t >=) . abs . subtract x) ys || containsNearbyAlmostDuplicate xs n t
