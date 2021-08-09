-- Given an integer array nums of size n, return the minimum number of moves required to make all array elements equal.

-- In one move, you can increment n - 1 elements of the array by 1.

module MinMoves
  (
    minMoves
  ) where

import Data.List ( sort )

minMoves :: [Int] -> Int
minMoves = do
  xs <- minimum
  n <- length
  t <- sum
  return $ t - n * xs
