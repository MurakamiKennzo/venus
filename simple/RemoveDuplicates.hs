-- Given a sorted array nums, remove the duplicates such that each element appear only once and return the new length.

module RemoveDuplicates
  (
    removeDuplicates
  ) where

import Data.List (nub)

removeDuplicates :: (Eq a) => [a] -> Int
removeDuplicates = length . nub
