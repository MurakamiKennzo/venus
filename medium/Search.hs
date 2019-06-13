-- Suppose an array sorted in ascending order is rotated at some pivot unknown to you beforehand.

-- (i.e., [0,1,2,4,5,6,7] might become [4,5,6,7,0,1,2]).

-- You are given a target value to search. If found in the array return its index, otherwise return -1.

module Search
  (
    search
  ) where

import Data.List (elemIndex)

search :: [Int] -> Int -> Int
search xs a = case elemIndex a xs of
              Nothing -> -1
              Just x -> x
