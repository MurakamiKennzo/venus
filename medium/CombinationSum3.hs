-- Find all possible combinations of k numbers that add up to a number n, given that only numbers from 1 to 9 can be used and each combination should be a unique set of numbers.

-- Note:

-- All numbers will be positive integers.
-- The solution set must not contain duplicate combinations.

module CombinationSum3
  (
    combinationSum3
  ) where

combinationSum3 :: Int -> Int -> [[Int]]
combinationSum3 = combinationSum3' [1 .. 9]

combinationSum3' :: [Int] -> Int -> Int -> [[Int]]
combinationSum3' _ 0 _ = []
combinationSum3' [] _ _ = []
combinationSum3' xs 1 n = map return . filter (== n) $ xs
combinationSum3' (x:xs) k n = let a = combinationSum3' xs (pred k) (n - x)
                                  b = combinationSum3' xs k n
                              in  map (x:) a <> b
