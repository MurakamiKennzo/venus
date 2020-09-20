-- Given a n x n matrix where each of the rows and columns are sorted in ascending order, find the kth smallest element in the matrix.

-- Note that it is the kth smallest element in the sorted order, not the kth distinct element.

module KthSmallest2
  (
    kthSmallest
  ) where

import Data.List ( sort )

kthSmallest :: (Ord a) => Matrix a -> Int -> a
kthSmallest matrix k = (!! (k - 1)) . sort $ foldr (<>) mempty matrix

type Matrix a = [[a]]
