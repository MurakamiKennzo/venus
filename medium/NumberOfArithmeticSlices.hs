-- An integer array is called arithmetic if it consists of at least three elements and if the difference between any two consecutive elements is the same.

-- For example, [1,3,5,7,9], [7,7,7,7], and [3,-1,-5,-9] are arithmetic sequences.
-- Given an integer array nums, return the number of arithmetic subarrays of nums.

-- A subarray is a contiguous subsequence of the array.

module NumberOfArithmeticSlices
  (
    numberOfArithmeticSlices
  ) where

numberOfArithmeticSlices :: (Num a, Eq a) => [a] -> Int
numberOfArithmeticSlices xs = numberOfArithmeticSlices' (2, length xs) 2 xs

numberOfArithmeticSlices' :: (Num a, Eq a) => (Int, Int) -> Int -> [a] -> Int
numberOfArithmeticSlices' (a, b) c xs
  | a >= b = count c
  | xs !! a - xs !! (a - 1) == xs !! (a - 1) - xs !! (a - 2) = numberOfArithmeticSlices' (succ a, b) (succ c) xs
  | otherwise = count c + numberOfArithmeticSlices' (succ a, b) 2 xs
  where count :: Int -> Int
        count n = (`div` 2) $ (n - 2) * (n - 1)
