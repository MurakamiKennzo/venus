-- Given a m x n grid filled with non-negative numbers, find a path from top left to bottom right which minimizes the sum of all numbers along its path.

-- Note: You can only move either down or right at any point in time.

module MinPathSum
  (
    minPathSum
  ) where

minPathSum :: [[Int]] -> Int
minPathSum (x:[]) = sum x
minPathSum a@([x]:xs) = sum . map (!! 0) $ a
minPathSum a@(x:xs) = let h = head x
                          right = minPathSum $ map (drop 1) a
                          bottom = minPathSum xs
                      in h + min right bottom
