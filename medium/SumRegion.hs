-- Given a 2D matrix matrix, find the sum of the elements inside the rectangle defined by its upper left corner (row1, col1) and lower right corner (row2, col2).

-- [
--   [3, 0, 1, 4, 2],
--   [5, 6, 3, 2, 1],
--   [1, 2, 0, 1, 5],
--   [4, 1, 0, 1, 7],
--   [1, 0, 3, 0, 5]
-- ]

-- The above rectangle (with the red border) is defined by (row1, col1) = (2, 1) and (row2, col2) = (4, 3), which contains sum = 8.

module SumRegion
  (
    sumRegion
  ) where

sumRegion :: (Num a) => (Int, Int) -> (Int, Int) -> Matrix a -> a
sumRegion (a, b) (c, d) = sum . map (sumRange (b, d)) . slice (a, c)

sumRange :: (Num a) => (Int, Int) -> [a] -> a
sumRange (a, b) = sum . slice (a, b)

slice :: (Int, Int) -> [a] -> [a]
slice (a, b) = take (b - (max 0 a) + 1) . drop a

type Matrix a = [[a]]
