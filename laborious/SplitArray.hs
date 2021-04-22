-- Given an array nums which consists of non-negative integers and an integer m, you can split the array into m non-empty continuous subarrays.

-- Write an algorithm to minimize the largest sum among these m subarrays.

module SplitArray
  (
    splitArray
  ) where

splitArray :: [Int] -> Int -> Int
splitArray xs m = splitArray' (maximum xs, sum xs) xs m

splitArray' :: (Int, Int) -> [Int] -> Int -> Int
splitArray' (a, b) xs m
  | a == b = a
  | m' <= m = splitArray' (a, c) xs m
  | otherwise = splitArray' (c + 1, b) xs m
  where c = (a + b) `div` 2
        m' = splitArray'' c xs 0

splitArray'' :: Int -> [Int] -> Int -> Int
splitArray'' _ [] _ = 1
splitArray'' a (x:xs) b
  | b + x > a = 1 + splitArray'' a xs x
  | otherwise = splitArray'' a xs (b + x)
