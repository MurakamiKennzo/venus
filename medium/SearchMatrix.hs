-- Write an efficient algorithm that searches for a value in an m x n matrix. This matrix has the following properties:

-- Integers in each row are sorted from left to right.
-- The first integer of each row is greater than the last integer of the previous row.

module SearchMatrix
  (
    searchMatrix
  ) where

import Data.List ( find )

searchMatrix :: (Ord a, Num a) => Matrix a -> a -> Bool
searchMatrix m a = case find ( ( >= a) . last ) m >>= find (== a) of
                      Nothing -> False
                      _ -> True

type Matrix a = [[a]]
