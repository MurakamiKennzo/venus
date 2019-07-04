-- You are given an n x n 2D matrix representing an image.

-- Rotate the image by 90 degrees (clockwise).

module Rotate
  (
    rotate
  ) where

type Matrix = [[Int]]

rotate :: Matrix -> Matrix
rotate a = let max = length a - 1 in map (\i -> reverse . map (!! i) $ a) [0 .. max]
