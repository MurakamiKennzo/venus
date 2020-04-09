-- Write an efficient algorithm that searches for a value in an m x n matrix. This matrix has the following properties:

-- Integers in each row are sorted in ascending from left to right.
-- Integers in each column are sorted in ascending from top to bottom.

module SearchMatrix2
  (
    searchMatrix
  ) where

searchMatrix :: (Eq a) => Matrix a -> a -> Bool 
searchMatrix [] _ = False
searchMatrix (x:xs) y
  | y `elem` x = True
  | otherwise = searchMatrix xs y

type Matrix a = [[a]]
