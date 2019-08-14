-- Given two sorted integer arrays nums1 and nums2, merge nums2 and nums1 as one sorted array.

module Merge
  (
    merge
  ) where

import Data.List (sort)

merge :: (Ord a) => [a] -> [a] -> [a]
merge a b = sort $ a ++ b
