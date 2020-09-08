-- Given a set of distinct positive integers, find the largest subset such that every pair (Si, Sj) of elements in this subset satisfies:

-- Si % Sj = 0 or Sj % Si = 0.

-- If there are multiple solutions, return any subset is fine.

module LargestDivisibleSubset
  (
    largestDivisibleSubset
  ) where

import Data.List ( maximumBy
                 , sort )
import Data.Function ( on )

largestDivisibleSubset :: [Int] -> [Int]
largestDivisibleSubset = maximumBy (compare `on` length) . largestDivisibleSubset' [] . sort

largestDivisibleSubset' :: [[Int]] -> [Int] -> [[Int]]
largestDivisibleSubset' a [] = a
largestDivisibleSubset' [] (x:xs) = largestDivisibleSubset' [[x]] xs
largestDivisibleSubset' xs (y:ys) = largestDivisibleSubset' (fit y xs) ys

fit :: Int -> [[Int]] -> [[Int]]
fit y [] = [[y]]
fit y (x:xs)
  | y `mod` last x == 0 = x:(x ++ [y]): fit y xs
  | otherwise = x: fit y xs
