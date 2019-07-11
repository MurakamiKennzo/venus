-- Given a matrix of m x n elements (m rows, n columns), return all elements of the matrix in spiral order.

module SpiralOrder
  (
    spiralOrder
  ) where

type Matrix a = [[a]]

spiralOrder :: Matrix a -> [a]
spiralOrder [] = []
spiralOrder a = readOut a ++ (spiralOrder . innerMatrix $ a)

readOut :: Matrix a -> [a]
readOut [] = []
readOut [a] = a
readOut a@(x:xs) = top ++ right ++ bottom ++ left
  where top = x
        right = map (!! (length x - 1)) . init $ xs
        bottom = reverse . last $ xs
        left = reverse . map (!! 0) . init $ xs

innerMatrix :: Matrix a -> Matrix a
innerMatrix [] = []
innerMatrix [a] = []
innerMatrix (x:xs) = map (drop 1) . map (take a) . init $ xs
  where a = length x - 1
