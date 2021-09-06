-- Given an integer array nums of size n, return the minimum number of moves required to make all array elements equal.

-- In one move, you can increment or decrement an element of the array by 1.

-- Test cases are designed so that the answer will fit in a 32-bit integer.

module MinMoves2
  (
    minMoves2
  ) where

import Data.List ( sort )

minMoves2 :: [Int] -> Int
minMoves2 xs = foldr (\a b -> (b+) $ abs $ a - x) 0 xs'
  where xs' = sort xs
        x = xs' !! (length xs `div` 2)
