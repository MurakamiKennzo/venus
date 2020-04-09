-- Given an array nums, there is a sliding window of size k which is moving from the very left of the array to the very right. You can only see the k numbers in the window. Each time the sliding window moves right by one position. Return the max sliding window.

module MaxSlidingWindow
  (
    maxSlidingWindow
  ) where

maxSlidingWindow :: (Ord a) => [a] -> Int -> [a]
maxSlidingWindow a n
  | length a <= n = [maximum a]
  | otherwise = (maximum . take n $ a) : (flip maxSlidingWindow n . tail $ a)
