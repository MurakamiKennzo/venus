-- Given an unsorted array of integers, find the length of the longest consecutive elements sequence.

module LongestConsecutive
  (
    longestConsecutive
  ) where

import Data.List ( sort
                 , sortOn )

longestConsecutive :: [Int] -> Int
longestConsecutive [] = 0
longestConsecutive a = last . sort . map length . groupBy' (\a b -> succ a == b) . sort $ a

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' _ [a] = [[a]]
groupBy' f (x:y:z)
  | f x y = (x: head a):tail a
  | otherwise = [x] : a
  where a = groupBy' f (y:z)
