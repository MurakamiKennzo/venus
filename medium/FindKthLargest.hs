-- Find the kth largest element in an unsorted array. Note that it is the kth largest element in the sorted order, not the kth distinct element.

module FindKthLargest
  (
    findKthLargest
  ) where

import Data.List ( sort )

findKthLargest :: (Ord a) => [a] -> Int -> a
findKthLargest xs k = (!! pred k) . reverse $ sort xs
