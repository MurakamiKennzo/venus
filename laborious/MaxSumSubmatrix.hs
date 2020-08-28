-- Given a non-empty 2D matrix matrix and an integer k, find the max sum of a rectangle in the matrix such that its sum is no larger than k.

module MaxSumSubmatrix
  (
    maxSumSubmatrix
  ) where

import Data.List ( sort
                 , find )
import Data.Maybe ( fromJust )

maxSumSubmatrix :: (Num a, Ord a) => Matrix a -> a -> a
maxSumSubmatrix matrix n = fromJust . find (<= n) . reverse . sort . map sum . map (map sum) $ matrix' matrix

matrix' :: Matrix a -> [Matrix a]
matrix' [] = []
matrix' a@[[x]] = [a]
matrix' ([]:_) = []
matrix' matrix@(x:_) = matrix : matrix' up <> matrix' right <> matrix' down <> matrix' left
  where up = tail matrix
        right = map (take (length x - 1)) matrix
        down = init matrix
        left = map (drop 1) matrix

type Matrix a = [[a]]
