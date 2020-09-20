-- Given an integer array with all positive numbers and no duplicates, find the number of possible combinations that add up to a positive integer target.

module CombinationSum4
  (
    combinationSum4
  ) where

combinationSum4 :: [Int] -> Int -> Int
combinationSum4 xs n 
  | n < 0 = 0
  | n == 0 = 1
  | otherwise = sum . map (combinationSum4 xs) $ [ n - x | x <- xs ]
