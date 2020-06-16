--  You are given an integer array nums and you have to return a new counts array. The counts array has the property where counts[i] is the number of smaller elements to the right of nums[i].

module CountSmaller
  (
    countSmaller
  ) where

countSmaller :: (Ord a) => [a] -> [Int]
countSmaller [] = []
countSmaller (x:xs) = (length . filter (< x) $ xs) : countSmaller xs
